package tests

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.signature.ClassFileSignatures
import scala.meta.internal.metals.signature.JvmSignatureParser
import scala.meta.internal.metals.signature.JvmSignatureParser.{
  JClass,
  JPrim,
  JVar,
}
import scala.meta.internal.metals.signature.SignatureSearchProvider
import scala.meta.internal.semanticdb.Access
import scala.meta.internal.semanticdb.ClassSignature
import scala.meta.internal.semanticdb.ConstantType
import scala.meta.internal.semanticdb.IntConstant
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.MethodSignature
import scala.meta.internal.semanticdb.Range
import scala.meta.internal.semanticdb.Scope
import scala.meta.internal.semanticdb.Schema
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.Type
import scala.meta.internal.semanticdb.TypeRef
import scala.meta.internal.semanticdb.ValueSignature
import scala.meta.io.AbsolutePath

class SignatureSearchSuite extends BaseSuite {

  private implicit val ec: ExecutionContext = ExecutionContext.global

  // A placeholder prefix; the matcher never inspects it.
  private val noPrefix: Type = ConstantType(IntConstant(0))

  private def typeRef(sym: String): Type =
    TypeRef(noPrefix, sym, Nil)

  private def methodSig(
      params: Seq[Type],
      ret: Type,
  ): MethodSignature = {
    val paramInfos = params.map { t =>
      SymbolInformation(
        "param/",
        Language.SCALA,
        Kind.LOCAL,
        0,
        "p",
        ValueSignature(t),
        Nil,
        Access.Empty,
        Nil,
        None,
      )
    }
    MethodSignature(
      typeParameters = None,
      parameterLists = List(Scope(Nil, paramInfos)),
      returnType = ret,
      throws = Nil,
    )
  }

  /** A real definition occurrence, matching what the compiler emits for anything actually written in source. */
  private def definitionOccurrence(symbol: String): SymbolOccurrence =
    SymbolOccurrence(
      range = Some(Range(0, 0, 0, 0)),
      symbol = symbol,
      role = SymbolOccurrence.Role.DEFINITION,
    )

  private def makeDoc(
      uri: String,
      symbols: Seq[SymbolInformation],
  ): TextDocuments =
    TextDocuments(
      symbols.map { s =>
        TextDocument(
          schema = Schema.SEMANTICDB4,
          uri = uri,
          text = "",
          md5 = "",
          language = Language.SCALA,
          symbols = List(s),
          occurrences = List(definitionOccurrence(s.symbol)),
          diagnostics = Nil,
        )
      }
    )

  private def fakePath(str: String) = AbsolutePath(s"/tmp/fake/$str")

  private def provider: SignatureSearchProvider =
    new SignatureSearchProvider(
      null.asInstanceOf[BuildTargets],
      AbsolutePath("/tmp/fake-workspace"),
    )

  private def search(p: SignatureSearchProvider, q: String): List[String] =
    Await.result(p.search(q), Duration.Inf).map(_.getName)

  // -- JvmSignatureParser: method signature parsing ---------------------------

  test("parse method: simple no-arg void") {
    val result = JvmSignatureParser.parseMethod("()V").get
    assertEquals(result.typeParams, Nil)
    assertEquals(result.params, Nil)
    assertEquals(result.ret, JPrim("Unit"))
  }

  test("parse method: single object param, int return") {
    val result = JvmSignatureParser.parseMethod("(Ljava/lang/String;)I").get
    assertEquals(result.typeParams, Nil)
    assertEquals(result.params.size, 1)
    result.params.head match {
      case JClass(name, args) =>
        assertEquals(name, "java/lang/String")
        assertEquals(args, Nil)
      case other => fail(s"Expected JClass, got $other")
    }
    assertEquals(result.ret, JPrim("Int"))
  }

  test("parse method: generic with type variable in return") {
    val sig = "<A:Ljava/lang/Object;>(LA;)LA;"
    val result = JvmSignatureParser.parseMethod(sig).get
    assertEquals(result.typeParams, List("A"))
    assertEquals(result.params.size, 1)
    result.params.head match {
      case JClass(name, _) => assertEquals(name, "A")
      case other => fail(s"Expected JClass, got $other")
    }
    result.ret match {
      case JClass(name, _) => assertEquals(name, "A")
      case other => fail(s"Expected JClass, got $other")
    }
  }

  test("parse method: generic with nested HKT args") {
    // <A:Ljava/lang/Object;>(Lcats/effect/IO<TA;>;)Lcats/effect/IO<TA;>;
    val sig =
      "<A:Ljava/lang/Object;>(Lcats/effect/IO<TA;>;)Lcats/effect/IO<TA;>;"
    val result = JvmSignatureParser.parseMethod(sig).get
    assertEquals(result.typeParams, List("A"))
    assertEquals(result.params.size, 1)
    result.params.head match {
      case JClass(name, args) =>
        assertEquals(name, "cats/effect/IO")
        assertEquals(args.size, 1)
        args.head match {
          case JVar(v) => assertEquals(v, "A")
          case other => fail(s"Expected JVar, got $other")
        }
      case other => fail(s"Expected JClass, got $other")
    }
  }

  test("parse method: two type params, multiple params and return") {
    val sig =
      "<A:Ljava/lang/Object;B:Ljava/lang/Object;>" +
        "(Lcats/effect/IO<TA;>;Lcats/effect/IO<TB;>;)Lcats/effect/IO<TB;>;"
    val result = JvmSignatureParser.parseMethod(sig).get
    assertEquals(result.typeParams, List("A", "B"))
    assertEquals(result.params.size, 2)
  }

  test("parse method: malformed returns None") {
    assertEquals(JvmSignatureParser.parseMethod("garbage!!"), None)
  }

  // -- JvmSignatureParser: class signature parsing ----------------------------

  test("parse class: no type params") {
    val result = JvmSignatureParser.parseClass("Ljava/lang/Object;").get
    assertEquals(result.typeParams, Nil)
  }

  test("parse class: one type param") {
    val result =
      JvmSignatureParser
        .parseClass("<T:Ljava/lang/Object;>Ljava/lang/Object;")
        .get
    assertEquals(result.typeParams, List("T"))
  }

  test("parse class: two type params with bounds") {
    val sig =
      "<A:Ljava/lang/Object;B:Ljava/lang/Comparable<Ljava/lang/Object;>;>Ljava/lang/Object;"
    val result = JvmSignatureParser.parseClass(sig).get
    assertEquals(result.typeParams, List("A", "B"))
  }

  // -- JvmSignatureParser: descriptor parsing ---------------------------------

  test("parse descriptor: simple") {
    val result = JvmSignatureParser.parseDescriptor("(I)Z").get
    assertEquals(result._1.size, 1)
    assertEquals(result._1.head, JPrim("Int"))
    assertEquals(result._2, JPrim("Boolean"))
  }

  test("parse descriptor: void return") {
    val result = JvmSignatureParser.parseDescriptor("()V").get
    assertEquals(result._1, Nil)
    assertEquals(result._2, JPrim("Unit"))
  }

  test("parse descriptor: class param and return") {
    val result =
      JvmSignatureParser
        .parseDescriptor("(Lcats/effect/IO;)Lcats/effect/IO;")
        .get
    assertEquals(result._1.size, 1)
    result._1.head match {
      case JClass(name, args) =>
        assertEquals(name, "cats/effect/IO")
        assertEquals(args, Nil)
      case other => fail(s"Expected JClass, got $other")
    }
  }

  test("parse descriptor: array param") {
    val result = JvmSignatureParser.parseDescriptor("([I)I").get
    assertEquals(result._1.size, 1)
  }

  test("search finds method by return type") {
    val p = provider
    val doc = makeDoc(
      "foo.scala",
      Seq(
        SymbolInformation(
          "a/MyClass#compute().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "compute",
          methodSig(
            params = List(typeRef("scala/String#")),
            ret = typeRef("scala/collection/immutable/List#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("foo.scala"))

    // Inkuire queries describe a full signature shape, not "mentions this
    // type somewhere" — write it like the method's own type.
    val results = search(p, "String => List")
    assertEquals(results, List("compute"))
  }

  test("search matches by parameter type") {
    val p = provider
    val doc = makeDoc(
      "bar.scala",
      Seq(
        SymbolInformation(
          "b/Handler#handle().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "handle",
          methodSig(
            params = List(typeRef("b/Request#")),
            ret = typeRef("scala/Unit#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("bar.scala"))

    val results = search(p, "Request => Unit")
    assertEquals(results, List("handle"))
  }

  test("search requires the exact signature shape") {
    val p = provider
    val doc = makeDoc(
      "baz.scala",
      Seq(
        SymbolInformation(
          "c/Service#doIt().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "doIt",
          methodSig(
            params = List(typeRef("c/Request#")),
            ret = typeRef("c/Response#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
        SymbolInformation(
          "c/Other#other().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "other",
          methodSig(
            params = List(typeRef("c/Request#")),
            ret = typeRef("scala/Int#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
      ),
    )
    p.onChange(doc, fakePath("baz.scala"))

    // Only `doIt` has exactly this shape; `other` returns Int instead of Response.
    val results = search(p, "Request => Response")
    assertEquals(results, List("doIt"))
  }

  test(
    "search multi-arg method by argument/result shape, comma or arrow syntax"
  ) {
    // Mirrors argos-server's AccountLogic.getMemberDetailsSummary:
    //   def getMemberDetailsSummary(user: User, skipCache: Boolean = false): Box[MemberDetailsSummary]
    // Both syntaxes must find it without naming the enclosing type:
    //   - comma form: top-level commas are rewritten to `=>` by the provider
    //   - arrow form: Inkuire's native multi-argument query shape
    val p = provider
    val boxType = TypeRef(
      noPrefix,
      "net/liftweb/common/Box#",
      List(typeRef("com/topgolf/argos/model/MemberDetailsSummary#")),
    )
    // Register the enclosing trait with a real ClassSignature so the
    // provider has a genuine class-type receiver available — this is the
    // regression this test guards: the method must still be findable by
    // argument/result shape *without* naming `AccountLogic`.
    val doc = makeDoc(
      "account.scala",
      Seq(
        SymbolInformation(
          "com/topgolf/argos/logic/AccountLogic#",
          Language.SCALA,
          Kind.TRAIT,
          0,
          "AccountLogic",
          ClassSignature(None, Nil, Type.Empty, None),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
        SymbolInformation(
          "com/topgolf/argos/logic/AccountLogic#getMemberDetailsSummary().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "getMemberDetailsSummary",
          methodSig(
            params = List(
              typeRef("com/topgolf/dataservice/User#"),
              typeRef("scala/Boolean#"),
            ),
            ret = boxType,
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
      ),
    )
    p.onChange(doc, fakePath("account.scala"))

    assertEquals(
      search(p, "User, Boolean => Box[MemberDetailsSummary]"),
      List("getMemberDetailsSummary"),
    )
    assertEquals(
      search(p, "User => Boolean => Box[MemberDetailsSummary]"),
      List("getMemberDetailsSummary"),
    )
  }

  test("comma normalization preserves commas inside type arguments") {
    // A comma inside `[...]` (type args) must NOT be rewritten to `=>`.
    // Register a method whose parameter is Map[String, Int] (two type args
    // separated by a comma). The query uses the same comma and must match.
    val p = provider
    val mapType = TypeRef(
      noPrefix,
      "scala/collection/immutable/Map#",
      List(
        typeRef("scala/String#"),
        typeRef("scala/Int#"),
      ),
    )
    val doc = makeDoc(
      "maptest.scala",
      Seq(
        SymbolInformation(
          "f/Repo#lookup().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "lookup",
          methodSig(
            params = List(mapType),
            ret = typeRef("scala/Int#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("maptest.scala"))

    // The comma inside Map[String, Int] is preserved (not converted to =>).
    assertEquals(search(p, "Map[String, Int] => Int"), List("lookup"))
  }

  test("search returns empty for no match") {
    val p = provider
    val results = search(p, "=> NonExistentType")
    assertEquals(results, Nil)
  }

  test("empty query returns all indexed methods") {
    val p = provider
    val doc = makeDoc(
      "all.scala",
      Seq(
        SymbolInformation(
          "a/First#methodA().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "methodA",
          methodSig(
            params = List(typeRef("scala/Int#")),
            ret = typeRef("scala/Int#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
        SymbolInformation(
          "a/Second#methodB().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "methodB",
          methodSig(
            params = List(typeRef("b/Request#")),
            ret = typeRef("b/Response#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        ),
      ),
    )
    p.onChange(doc, fakePath("all.scala"))

    // Empty query returns all indexed methods
    val allResults = search(p, "")
    assertEquals(allResults.toSet, Set("methodA", "methodB"))

    // Specific query narrows results
    val filtered = search(p, "Request => Response")
    assertEquals(filtered, List("methodB"))
  }

  test("onDelete removes method") {
    val p = provider
    val doc = makeDoc(
      "tmp.scala",
      Seq(
        SymbolInformation(
          "d/Temp#method().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "method",
          methodSig(
            params = Nil,
            ret = typeRef("scala/Int#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("tmp.scala"))
    assertEquals(search(p, "=> Int"), List("method"))

    p.onDelete(fakePath("tmp.scala"))
    assertEquals(search(p, "=> Int"), Nil)
  }

  test("nested generics expand type args") {
    val p = provider
    val mapType = TypeRef(
      noPrefix,
      "scala/collection/immutable/Map#",
      List(
        typeRef("scala/String#"),
        typeRef("scala/collection/immutable/List#"),
      ),
    )
    val doc = makeDoc(
      "nested.scala",
      Seq(
        SymbolInformation(
          "e/Repo#get().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "get",
          methodSig(params = Nil, ret = mapType),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("nested.scala"))

    // Free type variables (single letters) unify with each type argument
    // independently, proving both args are modeled, not just the outer Map.
    assertEquals(search(p, "=> Map[A, B]"), List("get"))
    // Wrong arity (Map only takes 2 type args here) must not match.
    assertEquals(search(p, "=> Map[A]"), Nil)
  }

  // -- ClassFileSignatures: descriptor parsing --------------------------------

  test("descriptor: object param + primitive return") {
    assertEquals(
      ClassFileSignatures.typeNames("(Ljava/lang/String;)I"),
      Set("String", "Int"),
    )
  }

  test("descriptor: two object params, void return") {
    assertEquals(
      ClassFileSignatures.typeNames("(Lcom/example/Foo;Lcom/example/Bar;)V"),
      Set("Foo", "Bar"),
    )
  }

  test("descriptor: array type") {
    assertEquals(
      ClassFileSignatures.typeNames("([Ljava/lang/String;)V"),
      Set("String"),
    )
  }

  test("descriptor: erased generic returns Object and named return") {
    assertEquals(
      ClassFileSignatures.typeNames("(Ljava/lang/Object;)Ljava/util/List;"),
      Set("Object", "List"),
    )
  }

  test("descriptor: no reference or primitive types") {
    assertEquals(ClassFileSignatures.typeNames("()V"), Set.empty[String])
  }

  test("descriptor: nested arrays and multiple primitives") {
    assertEquals(
      ClassFileSignatures.typeNames("([I[J)Ljava/lang/Long;"),
      Set("Int", "Long"),
    )
  }

  // -- ClassFileSignatures: class-file parsing --------------------------------

  private def writeUtf8(out: java.io.DataOutputStream, s: String): Unit = {
    val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    out.writeShort(bytes.length)
    out.write(bytes)
  }

  /**
   * Build a minimal valid JVM class file with the given class name and
   * method (name, descriptor, isStatic) triples, each with no attributes.
   */
  private def buildMinimalClassFile(
      className: String,
      methods: List[(String, String, Boolean)],
  ): Array[Byte] = {
    import java.io.ByteArrayOutputStream
    import java.io.DataOutputStream

    val nameSlots = methods.indices.map(5 + _ * 2)
    val descSlots = methods.indices.map(6 + _ * 2)
    val cpCount = 4 + methods.size * 2 + 1

    val bos = new ByteArrayOutputStream()
    val out = new DataOutputStream(bos)
    out.writeInt(0xcafebabe.toInt)
    out.writeShort(0) // minor
    out.writeShort(52) // major
    out.writeShort(cpCount)
    out.writeByte(1); writeUtf8(out, className) // slot 1
    out.writeByte(7); out.writeShort(1) // slot 2: Class -> 1
    out.writeByte(1); writeUtf8(out, "java/lang/Object") // slot 3
    out.writeByte(7); out.writeShort(3) // slot 4: Class -> 3
    methods.foreach { case (name, desc, _) =>
      out.writeByte(1); writeUtf8(out, name)
      out.writeByte(1); writeUtf8(out, desc)
    }
    out.writeShort(0x0021) // access_flags: public + super
    out.writeShort(2) // this_class
    out.writeShort(4) // super_class
    out.writeShort(0) // interfaces
    out.writeShort(0) // fields
    out.writeShort(methods.size)
    methods.indices.foreach { i =>
      val (_, _, isStatic) = methods(i)
      val access = 0x0001 | (if (isStatic) 0x0008 else 0)
      out.writeShort(access)
      out.writeShort(nameSlots(i))
      out.writeShort(descSlots(i))
      out.writeShort(0) // no attributes
    }
    out.writeShort(0) // class attributes
    out.close()
    bos.toByteArray
  }

  test("class file: extracts method names and descriptors") {
    val bytes = buildMinimalClassFile(
      "Test",
      List(("foo", "()V", false), ("bar", "(I)Z", false)),
    )
    val descs = ClassFileSignatures.methodDescriptors(bytes)
    assertEquals(
      descs.toSet,
      Set(("foo", "()V", false), ("bar", "(I)Z", false)),
    )
  }

  test("class file: detects static methods") {
    val bytes = buildMinimalClassFile(
      "Test",
      List(("foo", "()V", false), ("bar", "(I)Z", true)),
    )
    val descs = ClassFileSignatures.methodDescriptors(bytes)
    val bar = descs.find(_._1 == "bar")
    assertEquals(bar.map(_._3), Some(true))
  }

  test("class file: skips <clinit>") {
    val bytes = buildMinimalClassFile(
      "Test",
      List(("<clinit>", "()V", true), ("real", "()I", false)),
    )
    val descs = ClassFileSignatures.methodDescriptors(bytes)
    assertEquals(descs.toSet, Set(("real", "()I", false)))
  }

  test("class file: empty for non-class bytes") {
    assertEquals(
      ClassFileSignatures.methodDescriptors(Array.empty[Byte]),
      Nil,
    )
  }

  test("search matches Predef.String alias") {
    val p = provider
    val doc = makeDoc(
      "predef.scala",
      Seq(
        SymbolInformation(
          "f/StringFn#transform().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "transform",
          methodSig(
            params = List(typeRef("scala/Predef.String#")),
            ret = typeRef("scala/Predef.String#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("predef.scala"))

    // In Scala 3, `String` is recorded as `scala/Predef.String#`;
    // simpleName must strip the owner prefix so it matches "String".
    val results = search(p, "String => String")
    assertEquals(results, List("transform"))
  }

  test("search resolves param types from symlinks in doc symbol table") {
    // Real-world SemanticDB stores same-file params as symlinks (name strings)
    // in the ParameterScope, with the full SymbolInformation in the document's
    // symbols list. This test mirrors that layout.
    val p = provider
    val paramSym = SymbolInformation(
      "f/StringFn#transform().(s)",
      Language.SCALA,
      Kind.LOCAL,
      0,
      "s",
      ValueSignature(typeRef("scala/Predef.String#")),
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    val methodSig = MethodSignature(
      typeParameters = None,
      parameterLists = List(
        Scope(List("f/StringFn#transform().(s)"), Nil) // symlinks, no hardlinks
      ),
      returnType = typeRef("scala/Predef.String#"),
      throws = Nil,
    )
    val methodSym = SymbolInformation(
      "f/StringFn#transform().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "transform",
      methodSig,
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    // Both symbols must be in the SAME document (as in a real .semanticdb file)
    val doc = TextDocuments(
      List(
        TextDocument(
          schema = Schema.SEMANTICDB4,
          uri = "symlink.scala",
          text = "",
          md5 = "",
          language = Language.SCALA,
          symbols = List(methodSym, paramSym),
          occurrences = List(definitionOccurrence(methodSym.symbol)),
          diagnostics = Nil,
        )
      )
    )
    p.onChange(doc, fakePath("symlink.scala"))

    // Param type String is only reachable via the doc symbol table lookup
    val results = search(p, "String => String")
    assertEquals(results, List("transform"))
  }

  test("synthetic case-class methods are filtered out") {
    // Case classes generate apply, <init>, and copy methods. These are
    // compiler-generated and should not appear in search results.
    val p = provider

    // A user-authored method that matches Int => MySpecialNumber
    val userMethod = SymbolInformation(
      "f/Tmp.convertToMySpecialNumber().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "convertToMySpecialNumber",
      methodSig(
        params = List(typeRef("scala/Int#")),
        ret = typeRef("f/Tmp.MySpecialNumber#"),
      ),
      Nil,
      Access.Empty,
      Nil,
      None,
    )

    // Synthetic methods on the case class (owner is a class, ends with #)
    val applyMethod = SymbolInformation(
      "f/Tmp.MySpecialNumber#apply().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "apply",
      methodSig(
        params = List(typeRef("scala/Int#")),
        ret = typeRef("f/Tmp.MySpecialNumber#"),
      ),
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    val initMethod = SymbolInformation(
      "f/Tmp.MySpecialNumber#`<init>`().",
      Language.SCALA,
      Kind.CONSTRUCTOR,
      0,
      "<init>",
      methodSig(
        params = List(typeRef("scala/Int#")),
        ret = typeRef("f/Tmp.MySpecialNumber#"),
      ),
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    val copyMethod = SymbolInformation(
      "f/Tmp.MySpecialNumber#copy().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "copy",
      methodSig(
        params = List(typeRef("scala/Int#")),
        ret = typeRef("f/Tmp.MySpecialNumber#"),
      ),
      Nil,
      Access.Empty,
      Nil,
      None,
    )

    val doc = makeDoc(
      "Tmp.scala",
      Seq(userMethod, applyMethod, initMethod, copyMethod),
    )
    p.onChange(doc, fakePath("Tmp.scala"))

    // Only the user-authored method should match; synthetic ones are excluded
    val results = search(p, "Int => MySpecialNumber")
    assertEquals(results, List("convertToMySpecialNumber"))
  }

  test("package name excludes the synthetic _empty_ package") {
    // Top-level code with no `package` declaration is owned by scalac's
    // `_empty_/` sentinel symbol, not a real package worth displaying.
    val p = provider
    val doc = makeDoc(
      "Tmp.scala",
      Seq(
        SymbolInformation(
          "_empty_/Tmp.convertToMySpecialNumber().",
          Language.SCALA,
          Kind.METHOD,
          0,
          "convertToMySpecialNumber",
          methodSig(
            params = List(typeRef("scala/Int#")),
            ret = typeRef("scala/Int#"),
          ),
          Nil,
          Access.Empty,
          Nil,
          None,
        )
      ),
    )
    p.onChange(doc, fakePath("Tmp.scala"))

    val results = Await.result(p.search("Int => Int"), Duration.Inf)
    assertEquals(results.map(_.getName), List("convertToMySpecialNumber"))
    assertEquals(results.map(_.getContainerName), List("Tmp"))
  }

  test("case-class members with no definition occurrence are excluded") {
    // `copy$default$N`, `toString`, `hashCode`, `equals`, `canEqual`, and the
    // `productXxx` family aren't reliably marked SYNTHETIC by semanticdb-scalac
    // (unlike `apply`/`copy`/`unapply`/`<init>`, which are excluded by name),
    // but none of them ever get a definition occurrence — there's no source
    // token to point at. A method search hit with no real location is useless,
    // so these must be excluded regardless of what marks them.
    val p = provider
    val realMethod = SymbolInformation(
      "g/Point#translate().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "translate",
      methodSig(
        params = List(typeRef("scala/Int#")),
        ret = typeRef("g/Point#"),
      ),
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    val copyDefault = SymbolInformation(
      "g/Point#copy$default$1().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "copy$default$1",
      methodSig(params = Nil, ret = typeRef("scala/Int#")),
      Nil,
      Access.Empty,
      Nil,
      None,
    )
    val toStringMethod = SymbolInformation(
      "g/Point#toString().",
      Language.SCALA,
      Kind.METHOD,
      0,
      "toString",
      methodSig(params = Nil, ret = typeRef("java/lang/String#")),
      Nil,
      Access.Empty,
      Nil,
      None,
    )

    val doc = TextDocuments(
      List(
        TextDocument(
          schema = Schema.SEMANTICDB4,
          uri = "Point.scala",
          text = "",
          md5 = "",
          language = Language.SCALA,
          symbols = List(realMethod, copyDefault, toStringMethod),
          // Only the user-written method has a definition occurrence; the two
          // compiler-generated ones don't, matching real semanticdb-scalac output.
          occurrences = List(definitionOccurrence(realMethod.symbol)),
          diagnostics = Nil,
        )
      )
    )
    p.onChange(doc, fakePath("Point.scala"))

    assertEquals(search(p, "Int => Point"), List("translate"))
    assertEquals(
      Await.result(p.search(""), Duration.Inf).map(_.getName),
      List("translate"),
    )
  }
}
