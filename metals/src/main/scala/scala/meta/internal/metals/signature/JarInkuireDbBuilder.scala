package scala.meta.internal.metals.signature

import java.nio.file.Path
import java.util.jar.JarFile
import scala.collection.mutable
import scala.util.control.NonFatal

import scala.meta.internal.metals.signature.inkuire.api.InkuireDb
import scala.meta.internal.metals.signature.inkuire.model.AnnotatedSignature
import scala.meta.internal.metals.signature.inkuire.model.Invariance
import scala.meta.internal.metals.signature.inkuire.model.ITID
import scala.meta.internal.metals.signature.inkuire.model.Signature
import scala.meta.internal.metals.signature.inkuire.model.SignatureContext
import scala.meta.internal.metals.signature.inkuire.model.Type
import scala.meta.internal.metals.signature.inkuire.model.TypeName
import scala.meta.internal.metals.signature.inkuire.model.TypeLike

import JvmSignatureParser._

/**
 * Builds an [[InkuireDb]] from JVM class files in jar archives.
 *
 * Each class found in the jars is registered in `types` with its superclass
 * and interfaces as parents. Each method is registered as an
 * [[AnnotatedSignature]] in `functions`.
 *
 * Generic type information is recovered from the `Signature` attribute when
 * available; otherwise the erased JVM descriptor is used. All type parameters
 * are treated as invariant — recovering Scala variance would require reading
 * pickled `ScalaSignature` (Scala 2) or TASTy (Scala 3), which is out of
 * scope for this builder.
 */
object JarInkuireDbBuilder {

  /**
   * Build an InkuireDb from all class files in the given jars.
   *
   * @return the InkuireDb and a map from method URI to the jar path that
   *         declared it, for building LSP locations.
   */
  def build(jars: Iterable[Path]): (InkuireDb, Map[String, Path]) = {
    val functions = mutable.ListBuffer.empty[AnnotatedSignature]
    val types = mutable.Map.empty[ITID, (Type, Seq[Type])]
    val locations = mutable.Map.empty[String, Path]

    jars.foreach(jar => buildFromJar(jar, functions, types, locations))

    InkuireDb(
      functions = functions.toList,
      types = types.toMap,
      implicitConversions = Seq.empty,
      typeAliases = Map.empty,
    ) -> locations.toMap
  }

  private def buildFromJar(
      jar: Path,
      functions: mutable.ListBuffer[AnnotatedSignature],
      types: mutable.Map[ITID, (Type, Seq[Type])],
      locations: mutable.Map[String, Path],
  ): Unit = {
    try {
      val jarFile = new JarFile(jar.toFile)
      try {
        val entries = jarFile.entries()
        while (entries.hasMoreElements) {
          val entry = entries.nextElement()
          if (!entry.isDirectory && entry.getName.endsWith(".class")) {
            val in = jarFile.getInputStream(entry)
            val bytes =
              try in.readAllBytes()
              finally in.close()
            buildFromClassFile(bytes, jar, types, functions, locations)
          }
        }
      } finally {
        jarFile.close()
      }
    } catch {
      case NonFatal(_) => ()
    }
  }

  private def buildFromClassFile(
      bytes: Array[Byte],
      jar: Path,
      types: mutable.Map[ITID, (Type, Seq[Type])],
      functions: mutable.ListBuffer[AnnotatedSignature],
      locations: mutable.Map[String, Path],
  ): Unit = {
    ClassFileSignatures.parseClassFile(bytes).foreach { ci =>
      val internalName = ci.internalName
      val simple = simpleName(internalName)
      val itid = ITID(internalName + "#", isParsed = false)
      val pkg = packageName(internalName)

      // Type parameters declared on the class itself
      val classTParams = ci.genericSignature
        .flatMap(JvmSignatureParser.parseClass)
        .map(_.typeParams)
        .getOrElse(Nil)

      // Build the Inkuire Type node for this class
      val classParams = classTParams.map { name =>
        Invariance(
          Type(
            name = TypeName(name),
            isVariable = true,
            itid = Some(ITID(s"$internalName#/$name", isParsed = false)),
          )
        )
      }
      val classType = Type(
        name = TypeName(simple),
        params = classParams,
        itid = Some(itid),
      )

      // Superclass + interfaces → parents for the ancestry graph. These are
      // internal JVM class names (strings), not parsed JVM types, so build the
      // Inkuire Type nodes directly rather than routing through toType.
      val parents = (ci.superClass.toList ++ ci.interfaces).map {
        parentInternal =>
          Type(
            name = TypeName(simpleName(parentInternal)),
            itid = Some(ITID(parentInternal + "#", isParsed = false)),
          )
      }

      types(itid) = (classType, parents)

      // Register each method
      ci.methods.foreach { m =>
        val sig = buildMethodSig(m, classTParams.toSet)
        val uri = s"jar:$internalName#${m.name}"
        functions += AnnotatedSignature(
          signature = sig,
          name = m.name,
          packageName = pkg,
          uri = uri,
          entryType = "def",
        )
        locations(uri) = jar
      }
    }
  }

  private def buildMethodSig(
      method: ClassFileSignatures.MethodInfo,
      classVars: Set[String],
  ): Signature = {
    // Jar methods are registered with `receiver = None` so they behave like
    // free functions in Inkuire's matching. This lets queries like
    // `IO[_] => Future[_]` match methods such as `def foo: IO[String] => Future[String]`
    // whose arity (receiver + args + result) matches the query's arity.
    method.genericSignature.flatMap(JvmSignatureParser.parseMethod) match {
      case Some(msig) =>
        val vars = classVars ++ msig.typeParams.toSet
        Signature(
          receiver = None,
          arguments = msig.params.map(p => toType(p, vars)),
          result = toType(msig.ret, vars),
          context = SignatureContext(vars = vars, constraints = Map.empty),
        )
      case None =>
        // Fall back to the erased JVM descriptor
        JvmSignatureParser.parseDescriptor(method.descriptor) match {
          case Some((params, ret)) =>
            Signature(
              receiver = None,
              arguments = params.map(p => toType(p, classVars)),
              result = toType(ret, classVars),
              context =
                SignatureContext(vars = classVars, constraints = Map.empty),
            )
          case None =>
            Signature(
              receiver = None,
              arguments = Seq.empty,
              result = Type(name = TypeName("<unresolved>")),
              context =
                SignatureContext(vars = classVars, constraints = Map.empty),
            )
        }
    }
  }

  /** Convert a [[JvmType]] to an Inkuire [[TypeLike]]. */
  private def toType(jt: JvmType, vars: Set[String]): TypeLike = jt match {
    case JClass(name, args) =>
      val simple = simpleName(name)
      Type(
        name = TypeName(simple),
        itid = Some(ITID(name + "#", isParsed = false)),
        isVariable = vars.contains(simple),
        params = args.map(a => Invariance(toType(a, vars))),
      )
    case JVar(name) =>
      Type(
        name = TypeName(name),
        isVariable = true,
        itid = Some(ITID(name, isParsed = false)),
      )
    case JArray(element) =>
      // Simplification: represent arrays as their element type.
      toType(element, vars)
    case JPrim(name) =>
      Type(name = TypeName(name))
    case JWildcard =>
      Type.StarProjection
  }

  private def simpleName(internalName: String): String =
    internalName
      .split('/')
      .lastOption
      .map(_.split('$').last)
      .getOrElse(internalName)

  private def packageName(internalName: String): String = {
    val parts = internalName.split('/')
    if (parts.length > 1) parts.init.mkString(".") else ""
  }
}
