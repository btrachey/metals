package scala.meta.internal.metals.signature

import scala.collection.mutable

import scala.meta.internal.semanticdb.{Signature => _, Type => SType, _}
import scala.meta.internal.semanticdb.Scala.{DescriptorParser, Symbols}

import scala.meta.internal.metals.signature.inkuire.api.InkuireDb
import scala.meta.internal.metals.signature.inkuire.model._

/**
 * Converts compiled Scala 2 SemanticDB (`TextDocument`s) into Inkuire's
 * `InkuireDb` model — the same model Scala 3's Scaladoc builds from TASTy
 * behind the `-Ygenerate-inkuire` flag. See https://github.com/VirtusLab/Inkuire
 * and `dotty.tools.scaladoc.tasty.InkuireSupport` in the scala/scala3 repo,
 * which this mirrors as closely as SemanticDB's shape allows (ITID scheme,
 * currying, variance handling, implicit-conversion detection).
 *
 * Scope, matching that reference converter:
 *  - classes/traits get a `Type` (with real declaration-site variance on
 *    their type params) plus their direct parents, registered in `db.types`;
 *    `object`s/package objects do not (their members simply have no receiver).
 *  - abstract type members become orphan `types` entries (using the upper
 *    bound as a pseudo-parent, when it resolves to a concrete type); true
 *    aliases (`lowerBound == upperBound`) go into `db.typeAliases`.
 *  - every public, non-synthetic, non-override method/val becomes an
 *    `AnnotatedSignature` in `db.functions`; a def/val whose *result* is
 *    itself a function type is uncurried into flat arguments (`curry`,
 *    ported from the Scaladoc side, which also flattens multiple parameter
 *    lists directly since `parameterLists` are read as one flat sequence).
 *    Methods/vals are indexed with **no receiver** (consistent with the jar
 *    index), so Inkuire queries match by argument/result shape alone and the
 *    caller need not know the enclosing class — e.g. `User => Boolean =>
 *    Box[MemberDetailsSummary]` finds `getMemberDetailsSummary(user, skipCache)`.
 *  - implicit methods (hand-written `implicit def`s and the synthetic
 *    converters `implicit class` generates) are additionally recorded in
 *    `db.implicitConversions`.
 *
 * Deliberately out of scope, matching upstream TODOs in the Scala 3
 * converter: type bounds/constraints beyond a type member's own upper
 * bound, refinements, and existential/match-type internals (all collapsed
 * to their nearest concrete type).
 *
 * Only symbols *defined in the given document* get populated `types`
 * entries/ancestry; types referenced from elsewhere (stdlib, dependency
 * jars) appear only as external `ITID` references with no `types` entry.
 * That matches how Inkuire itself expects to be fed — `InkuireDb.withOrphanTypes`
 * stubs in anything referenced-but-undefined — and reflects that SemanticDB
 * is generated per source file, not for the whole classpath. Extending
 * coverage to dependency jars (which carry no SemanticDB) is a separate,
 * much larger problem (recovering class ancestry from bytecode or Scala
 * signature annotations) and is not attempted here.
 */
object InkuireDbBuilder {

  def fromTextDocument(doc: TextDocument): InkuireDb =
    new InkuireDbBuilder(doc).build()

  /**
   * Merge many per-file `InkuireDb`s into one.
   *
   * Deliberately NOT `InkuireDb.combineAll`/`InkuireDb.combine`: those call
   * `.distinct` on the accumulated `functions`/`types` at every pairwise fold
   * step, which is fine for a handful of files (e.g. the `TextDocument`s
   * within one compiled file) but quadratic-ish and untenable across a whole
   * workspace — tens of thousands of functions across a few thousand files
   * takes tens of seconds that way. A plain flatten is safe here because each
   * function/type is keyed by its unique SemanticDB symbol (`AnnotatedSignature.uri`,
   * `ITID.uuid`), which can't legitimately collide across files.
   */
  def combineAll(dbs: Iterable[InkuireDb]): InkuireDb =
    InkuireDb(
      functions = dbs.iterator.flatMap(_.functions).toSeq,
      types = dbs.iterator.flatMap(_.types).toMap,
      implicitConversions = dbs.iterator.flatMap(_.implicitConversions).toSeq,
      typeAliases = dbs.iterator.flatMap(_.typeAliases).toMap,
    )
}

private final class InkuireDbBuilder(doc: TextDocument) {

  private case class Container(
      classType: Option[Type],
      vars: Set[String],
      isModule: Boolean,
  )

  private val symtab: Map[String, SymbolInformation] =
    doc.symbols.iterator.map(i => i.symbol -> i).toMap

  private val containers: mutable.Map[String, Container] =
    mutable.Map
      .empty[String, Container]
      .withDefaultValue(
        Container(None, Set.empty, isModule = false)
      )

  private val UnresolvedType: Type =
    Type(
      name = TypeName("<unresolved>"),
      itid = Some(ITID("<unresolved>", isParsed = false)),
    )

  /** A bound type-lambda/polymorphic-method parameter, referenced as a variable placeholder. */
  private def typeLambdaArg(name: String): Type =
    Type(
      name = TypeName(name),
      itid = Some(ITID(s"external-type-lambda-arg-$name", isParsed = false)),
      isVariable = true,
    )

  private var db: InkuireDb = InkuireDb.empty

  def build(): InkuireDb = {
    doc.symbols.foreach(registerClassOrTrait)
    doc.symbols.foreach(registerTypeMember)
    doc.symbols.foreach(registerMethod)
    doc.symbols.foreach(registerValue)
    db
  }

  // ---------------------------------------------------------------------
  // classes / traits / objects
  // ---------------------------------------------------------------------

  private def registerClassOrTrait(info: SymbolInformation): Unit =
    info.signature match {
      case sig: ClassSignature if isVisible(info) =>
        val isModule = info.isObject || info.isPackageObject
        val vars = typeParamNames(sig.typeParameters)
        val t = classType(info, sig)
        if (isModule) {
          containers(info.symbol) = Container(None, vars, isModule = true)
        } else {
          containers(info.symbol) = Container(Some(t), vars, isModule = false)
          val parents = sig.parents
            .map(p => toTypeLike(p, vars))
            .collect { case p: Type => p }
          db = db.copy(types = db.types.updated(t.itid.get, (t, parents)))
        }
      case _ => ()
    }

  private def classType(info: SymbolInformation, sig: ClassSignature): Type =
    Type(
      name = TypeName(info.displayName),
      itid = Some(itidOf(info.symbol)),
      params = scopeSymbols(sig.typeParameters).map(typeParamVariance),
    )

  /** Build a class/method type parameter's own `Type` node, with real declaration-site variance. */
  private def typeParamVariance(tp: SymbolInformation): Variance = {
    val nestedArity = tp.signature match {
      case ts: TypeSignature => scopeSymbols(ts.typeParameters).size
      case _ => 0
    }
    val synthetic = (1 to nestedArity).map { i =>
      Type(
        name = TypeName(s"X$i"),
        itid = Some(ITID(s"synthetic-arg-$i-${tp.symbol}", isParsed = false)),
        isVariable = true,
      )
    }
    val self = Type(
      name = TypeName(tp.displayName),
      itid = Some(itidOf(tp.symbol)),
      isVariable = true,
      params = synthetic.map(Invariance(_)),
    )
    val wrapped: TypeLike =
      if (synthetic.isEmpty) self else TypeLambda(synthetic, self)
    if (tp.isCovariant) Covariance(wrapped)
    else if (tp.isContravariant) Contravariance(wrapped)
    else Invariance(wrapped)
  }

  // ---------------------------------------------------------------------
  // type members: aliases and abstract types
  // ---------------------------------------------------------------------

  private def registerTypeMember(info: SymbolInformation): Unit =
    info.signature match {
      case sig: TypeSignature if isVisible(info) && info.isType =>
        val ownerSym = ownerOf(info.symbol)
        val vars =
          containers(ownerSym).vars ++ typeParamNames(sig.typeParameters)
        val itid = itidOf(info.symbol)
        val t = Type(name = TypeName(info.displayName), itid = Some(itid))

        val isAlias =
          sig.lowerBound != SType.Empty && sig.lowerBound == sig.upperBound
        if (isAlias) {
          db = db.copy(
            types = db.types.updated(itid, (t, Seq.empty)),
            typeAliases =
              db.typeAliases.updated(itid, toTypeLike(sig.lowerBound, vars)),
          )
        } else {
          val parents = toTypeLike(sig.upperBound, vars) match {
            case p: Type if !p.itid.contains(itid) => Seq(p)
            case _ => Seq.empty
          }
          db = db.copy(types = db.types.updated(itid, (t, parents)))
        }
      case _ => ()
    }

  // ---------------------------------------------------------------------
  // methods
  // ---------------------------------------------------------------------

  private def registerMethod(info: SymbolInformation): Unit =
    info.signature match {
      case sig: MethodSignature
          if isVisible(info) && (info.isMethod || info.isConstructor) =>
        val ownerSym = ownerOf(info.symbol)
        val container = containers(ownerSym)
        val vars = container.vars ++ typeParamNames(sig.typeParameters)
        // No receiver: methods are indexed as free functions so that queries
        // like "User => Boolean => Box[...]" match by argument/result shape
        // without needing to know the enclosing class/trait. This aligns
        // with the jar index (JarInkuireDbBuilder), which also omits the
        // receiver.
        val receiver: Option[TypeLike] = None

        val arguments: Seq[TypeLike] = sig.parameterLists.flatMap { scope =>
          val params = scopeSymbols(Some(scope))
          // Drop whole implicit/using clauses; keep the params of any other clause,
          // flattening all (possibly curried) clauses into one argument list.
          if (params.nonEmpty && params.forall(_.isImplicit)) Seq.empty
          else params.map(paramType(_, vars))
        }

        val signature = curry(
          Signature(
            receiver = receiver,
            arguments = arguments,
            result = toTypeLike(sig.returnType, vars),
            context = SignatureContext(vars = vars, constraints = Map.empty),
          )
        )

        db = db.copy(functions =
          db.functions :+ AnnotatedSignature(
            signature = signature,
            name = info.displayName,
            packageName = dottedOwnerChain(ownerSym),
            uri = info.symbol,
            entryType = "def",
          )
        )

        if (info.isImplicit) registerImplicitConversion(sig, vars)
      case _ => ()
    }

  private def registerImplicitConversion(
      sig: MethodSignature,
      vars: Set[String],
  ): Unit = {
    val allParams =
      sig.parameterLists.flatMap(scope => scopeSymbols(Some(scope)))
    val from = allParams.headOption.collect {
      case p if p.signature.isInstanceOf[ValueSignature] =>
        toTypeLike(p.signature.asInstanceOf[ValueSignature].tpe, vars)
    }
    (from, toTypeLike(sig.returnType, vars)) match {
      case (Some(f), t: Type) =>
        db = db.copy(implicitConversions = db.implicitConversions :+ (f -> t))
      case _ => ()
    }
  }

  private def paramType(param: SymbolInformation, vars: Set[String]): TypeLike =
    param.signature match {
      case vs: ValueSignature => toTypeLike(vs.tpe, vars)
      case _ => UnresolvedType
    }

  // ---------------------------------------------------------------------
  // values (vals, not parameters — see the `info.isField` guard)
  // ---------------------------------------------------------------------

  private def registerValue(info: SymbolInformation): Unit =
    info.signature match {
      case sig: ValueSignature if isVisible(info) && info.isField =>
        val ownerSym = ownerOf(info.symbol)
        val container = containers(ownerSym)
        val vars = container.vars
        // No receiver: same rationale as registerMethod — search by type
        // shape, not by enclosing type.
        val receiver: Option[TypeLike] = None

        val signature = curry(
          Signature(
            receiver = receiver,
            arguments = Seq.empty,
            result = toTypeLike(sig.tpe, vars),
            context = SignatureContext(vars = vars, constraints = Map.empty),
          )
        )

        db = db.copy(functions =
          db.functions :+ AnnotatedSignature(
            signature = signature,
            name = info.displayName,
            packageName = dottedOwnerChain(ownerSym),
            uri = info.symbol,
            entryType = "val",
          )
        )
      case _ => ()
    }

  // ---------------------------------------------------------------------
  // Type/TypeLike conversion — the SemanticDB analogue of
  // `InkuireSupport.inner` (TASTy `TypeRepr` -> `Inkuire.TypeLike`).
  // ---------------------------------------------------------------------

  private val FunctionSymbol = "^scala/Function[0-9]+#$".r
  private val TupleSymbol = "^scala/Tuple[0-9]+#$".r

  private def toTypeLike(tpe: SType, vars: Set[String]): TypeLike = tpe match {
    case SType.Empty => UnresolvedType
    case UnionType(types) =>
      types.map(toTypeLike(_, vars)).reduceLeft(OrType(_, _))
    case IntersectionType(types) =>
      types.map(toTypeLike(_, vars)).reduceLeft(AndType(_, _))
    case WithType(types) =>
      types.map(toTypeLike(_, vars)).reduceLeft(AndType(_, _))
    case ByNameType(t) => toTypeLike(t, vars)
    case RepeatedType(t) => toTypeLike(t, vars) // TODO [Inkuire] repeated types
    case AnnotatedType(_, t) => toTypeLike(t, vars)
    case ExistentialType(t, _) =>
      toTypeLike(t, vars) // TODO [Inkuire] existentials
    case StructuralType(t, _) =>
      toTypeLike(t, vars) // TODO [Inkuire] refinements
    case MatchType(scrutinee, _) => toTypeLike(scrutinee, vars)
    case ThisType(symbol) => typeRefLike(symbol, Seq.empty, vars)
    case SuperType(_, symbol) => typeRefLike(symbol, Seq.empty, vars)

    case ConstantType(constant) =>
      val name = constantName(constant)
      Type(name = TypeName(name), itid = Some(ITID(name, isParsed = false)))

    case LambdaType(params, result) =>
      TypeLambda(
        scopeSymbolNames(params).map(typeLambdaArg),
        toTypeLike(result, vars),
      )
    case UniversalType(params, t) =>
      TypeLambda(
        scopeSymbolNames(params).map(typeLambdaArg),
        toTypeLike(t, vars),
      )

    case SingleType(_, symbol) =>
      // Best-effort: recover the singleton's widened type from the local symbol
      // table when it's defined in this file; otherwise reference it by name
      // (e.g. an external `Foo.type` where `Foo` is a dependency object).
      symtab.get(symbol) match {
        case Some(owner) =>
          owner.signature match {
            case vs: ValueSignature => toTypeLike(vs.tpe, vars)
            case _ => typeRefLike(symbol, Seq.empty, vars)
          }
        case None => typeRefLike(symbol, Seq.empty, vars)
      }

    case TypeRef(_, symbol, args)
        if args.nonEmpty && FunctionSymbol.matches(symbol) =>
      val name = s"Function${args.size - 1}"
      Type(
        name = TypeName(name),
        params = args.init.map(a => Contravariance(toTypeLike(a, vars))) :+
          Covariance(toTypeLike(args.last, vars)),
        itid = Some(ITID(s"${name}scala.${name}//[]", isParsed = false)),
      )
    case TypeRef(_, symbol, args)
        if args.nonEmpty && TupleSymbol.matches(symbol) =>
      val name = s"Tuple${args.size}"
      Type(
        name = TypeName(name),
        params = args.map(a => Covariance(toTypeLike(a, vars))),
        itid = Some(ITID(s"${name}scala.${name}//[]", isParsed = false)),
      )
    case TypeRef(prefix, symbol, args) =>
      typeRefLike(symbol, args, vars, pathDependentOwner(prefix))
  }

  /**
   * A `SingleType` prefix means this reference was reached through a specific
   * term path (`owner.Member`), not a plain top-level/package-qualified name
   * (which carries `Type.Empty` as its prefix) — the telltale shape of a
   * path-dependent type. The paradigm case is Scala 2's `Enumeration`: every
   * `object X extends Enumeration` shares the literal symbol
   * `scala/Enumeration#Value#` for `X.Value`, distinguished only by which
   * object it's accessed through. Without folding that owner into the type's
   * identity, `FeatureType.Value` and some unrelated enum's `Value` would
   * collapse into the same type.
   */
  private def pathDependentOwner(prefix: SType): Option[String] = prefix match {
    case SingleType(_, owner) => Some(owner)
    case _ => None
  }

  private def typeRefLike(
      symbol: String,
      args: Seq[SType],
      vars: Set[String],
      pathDependentOwner: Option[String] = None,
  ): Type = {
    val name = displayNameOf(symbol)
    val uuid = pathDependentOwner.fold(symbol)(owner => s"$symbol#$owner")
    val base = Type(
      name = TypeName(name),
      itid = Some(ITID(uuid, isParsed = false)),
      isVariable = vars.contains(name),
    )
    if (args.isEmpty) base
    else base.copy(params = args.map(a => Invariance(toTypeLike(a, vars))))
  }

  private def constantName(constant: Constant): String = constant match {
    case BooleanConstant(v) => v.toString
    case ByteConstant(v) => v.toString
    case ShortConstant(v) => v.toString
    case CharConstant(v) => v.toString
    case IntConstant(v) => v.toString
    case LongConstant(v) => v.toString
    case FloatConstant(v) => v.toString
    case DoubleConstant(v) => v.toString
    case StringConstant(v) => v
    case _ => "<unresolved>" // UnitConstant, NullConstant, Constant.Empty
  }

  /**
   * A def/val whose *result* is itself a function type (e.g. `def add: Int => Int => Int`,
   * or a curried definition surfacing as nested `FunctionN` results) gets flattened into
   * one flat argument list. Ported from `dotty.tools.scaladoc.Inkuire.curry` — it operates
   * purely on the Inkuire model, so it's identical for either producer.
   */
  private def curry(signature: Signature): Signature =
    signature.result.typ match {
      case t: Type if t.name.name == s"Function${t.params.size - 1}" =>
        curry(
          signature.copy(
            arguments = signature.arguments ++ t.params.init
              .map(_.typ)
              .map(Contravariance(_)),
            result = Covariance(t.params.last.typ),
          )
        )
      case _ => signature
    }

  // ---------------------------------------------------------------------
  // small SemanticDB helpers
  // ---------------------------------------------------------------------

  private def isVisible(info: SymbolInformation): Boolean =
    !info.isLocal &&
      !info.isSynthetic &&
      !info.isOverride &&
      !isLikelySyntheticCaseClassMember(info) &&
      isPublic(info.access)

  /**
   * `apply`/`copy`/`unapply`/`<init>` generated for a case class are not
   * reliably marked with the SYNTHETIC property by semanticdb-scalac, so they
   * must additionally be excluded by name — but only when owned directly by a
   * class (`#`), not a user-defined companion `object`/`def` (`.`).
   */
  private val syntheticCaseClassMemberNames =
    Set("<init>", "apply", "copy", "unapply")

  private def isLikelySyntheticCaseClassMember(
      info: SymbolInformation
  ): Boolean =
    syntheticCaseClassMemberNames.contains(info.displayName) && ownerOf(
      info.symbol
    ).endsWith("#")

  private def isPublic(access: Access): Boolean = access match {
    case Access.Empty => true
    case _: PublicAccess => true
    case _ => false
  }

  private def itidOf(symbol: String): ITID = ITID(symbol, isParsed = false)

  /** Safe owner-symbol extraction — a malformed/unexpected symbol shouldn't sink the whole file. */
  private def ownerOf(symbol: String): String =
    scala.util.Try(DescriptorParser(symbol)._2).getOrElse("")

  private def scopeSymbols(scope: Option[Scope]): Seq[SymbolInformation] =
    scope match {
      case None => Seq.empty
      case Some(sc) =>
        if (sc.hardlinks.nonEmpty) sc.hardlinks
        else sc.symlinks.flatMap(symtab.get)
    }

  private def scopeSymbolNames(scope: Option[Scope]): Seq[String] =
    scope match {
      case None => Seq.empty
      case Some(sc) =>
        if (sc.hardlinks.nonEmpty) sc.hardlinks.map(_.displayName)
        else
          sc.symlinks.map(sym =>
            symtab.get(sym).map(_.displayName).getOrElse(displayNameOf(sym))
          )
    }

  private def typeParamNames(scope: Option[Scope]): Set[String] =
    scopeSymbolNames(scope).toSet

  private def displayNameOf(symbol: String): String =
    symtab.get(symbol).map(_.displayName).getOrElse {
      scala.util.Try(DescriptorParser(symbol)._1.name.value).getOrElse(symbol)
    }

  private def dottedOwnerChain(sym: String): String =
    scala.util
      .Try {
        def loop(s: String, acc: List[String]): List[String] =
          // `_root_/` terminates the walk; `_empty_/` is scalac's sentinel for
          // top-level code with no `package` declaration — real, but not a
          // name worth showing, so drop it rather than emit "_empty_.Foo".
          if (
            s.isEmpty || s == Symbols.RootPackage || s == Symbols.EmptyPackage
          ) acc
          else {
            val (desc, owner) = DescriptorParser(s)
            if (owner == s || desc.name.value.isEmpty) acc
            else loop(owner, desc.name.value :: acc)
          }
        loop(sym, Nil).mkString(".")
      }
      .getOrElse("")
}
