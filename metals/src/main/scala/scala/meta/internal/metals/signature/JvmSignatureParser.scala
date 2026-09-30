package scala.meta.internal.metals.signature

/**
 * Parser for JVM generic signature strings (JVMS §4.7.9.2).
 *
 * Parses both method signatures and class-level signatures found in the
 * `Signature` attribute of `.class` files.
 *
 * Method signature format:
 * {{{
 *   <A:Ljava/lang/Object;B:Ljava/lang/Object;>
 *   (Lcats/Functor<TA;>;)
 *   Lcats/Functor<TA;>;
 * }}}
 *
 * Class signature format:
 * {{{
 *   <A:Ljava/lang/Object;>Lcats/effect/kernel/GenericEffect<...>;
 * }}}
 */
object JvmSignatureParser {

  // ── Intermediate representation ─────────────────────────────────────────────

  sealed trait JvmType

  /** A class reference. `internalName` is the JVM internal name (slash-separated), e.g. `cats/effect/IO`. */
  final case class JClass(internalName: String, args: List[JvmType])
      extends JvmType

  /** A type variable reference, e.g. `TA;` in the signature. */
  final case class JVar(name: String) extends JvmType

  /** An array type, e.g. `[I` = int[]. */
  final case class JArray(element: JvmType) extends JvmType

  /** A primitive type. `name` is the Scala display name ("Int", "Unit", etc.). */
  final case class JPrim(name: String) extends JvmType

  /** A wildcard type argument (`?`, `? extends T`, `? super T`). */
  case object JWildcard extends JvmType

  /** A parsed method generic signature. */
  final case class MethodSig(
      typeParams: List[String],
      params: List[JvmType],
      ret: JvmType,
  )

  /** The type parameters declared by a class (from the class-level `Signature` attribute). */
  final case class ClassSig(typeParams: List[String])

  // ── Constants ───────────────────────────────────────────────────────────────

  private val Prims: Map[Char, String] = Map(
    'B' -> "Byte",
    'C' -> "Char",
    'D' -> "Double",
    'F' -> "Float",
    'I' -> "Int",
    'J' -> "Long",
    'S' -> "Short",
    'Z' -> "Boolean",
    'V' -> "Unit",
  )

  // ── Public entry points ─────────────────────────────────────────────────────

  /**
   * Parse a method-level generic signature.
   *
   * @return the parsed signature, or `None` if the string is malformed
   */
  def parseMethod(sig: String): Option[MethodSig] =
    try Some(new P(sig).parseMethod())
    catch { case _: PErr => None }

  /**
   * Parse a class-level generic signature (extracts type parameters only).
   *
   * @return the declared type parameter names, or `None` if the string is malformed
   */
  def parseClass(sig: String): Option[ClassSig] =
    try Some(new P(sig).parseClass())
    catch { case _: PErr => None }

  /**
   * Parse an erased JVM method descriptor (e.g. `(Ljava/lang/String;)I`).
   *
   * @return (parameter types, return type), or `None` if the string is malformed
   */
  def parseDescriptor(desc: String): Option[(List[JvmType], JvmType)] =
    try { val (ps, rt) = new P(desc).parseDescriptor(); Some((ps, rt)) }
    catch { case _: PErr => None }

  // ── Internal parser ─────────────────────────────────────────────────────────

  private final class PErr(msg: String = "parse error") extends Exception(msg)

  private final class P(s: String) {
    private var i = 0
    private val len = s.length

    private def peek: Char = if (i < len) s.charAt(i) else '\u0000'
    private def eof: Boolean = i >= len
    private def bump(): Unit = { i += 1 }
    private def expect(c: Char): Unit =
      if (peek != c) throw new PErr(s"expected '$c' at pos $i, got '${peek}'")
      else bump()

    /** Parse a full method signature: `<TypeParams>(Params)Ret`. */
    def parseMethod(): MethodSig = {
      val tps = if (peek == '<') parseTypeParams() else Nil
      expect('(')
      val params = if (peek == ')') Nil else parseParamList()
      expect(')')
      val ret = parseType()
      MethodSig(tps, params, ret)
    }

    /** Parse a class-level signature: `<TypeParams> ClassType`. */
    def parseClass(): ClassSig = {
      val tps = if (peek == '<') parseTypeParams() else Nil
      ClassSig(tps)
    }

    /** Parse an erased descriptor: `(ParamTypes)ReturnType`. */
    def parseDescriptor(): (List[JvmType], JvmType) = {
      expect('(')
      val params = if (peek == ')') Nil else parseParamList()
      expect(')')
      val ret = parseType()
      (params, ret)
    }

    // ── Type parameters ──────────────────────────────────────────────────────

    private def parseTypeParams(): List[String] = {
      expect('<')
      val out = List.newBuilder[String]
      while (peek != '>' && !eof) {
        val name = readSimpleIdent()
        // Skip any bounds: :ClassType
        while (peek == ':') { bump(); parseType() }
        out += name
      }
      expect('>')
      out.result()
    }

    // ── Parameter lists ───────────────────────────────────────────────────────

    private def parseParamList(): List[JvmType] = {
      val out = List.newBuilder[JvmType]
      while (peek != ')' && !eof) {
        out += parseType()
      }
      out.result()
    }

    // ── Type expressions ──────────────────────────────────────────────────────

    def parseType(): JvmType = {
      // Count leading '[' for arrays
      var depth = 0
      while (peek == '[') { bump(); depth += 1 }
      val base = parseBaseType()
      (0 until depth).foldRight(base) { (_, acc) => JArray(acc) }
    }

    private def parseBaseType(): JvmType = peek match {
      case 'L' => parseClassType()
      case 'T' => parseTypeVar()
      case c if Prims.contains(c) =>
        bump()
        JPrim(Prims(c))
      case _ => throw new PErr(s"unexpected char '$peek' at pos $i")
    }

    private def parseClassType(): JvmType = {
      bump() // consume 'L'
      val name = readClassName()
      val args = if (peek == '<') parseTypeArgs() else Nil
      expect(';')
      JClass(name, args)
    }

    private def parseTypeVar(): JvmType = {
      bump() // consume 'T'
      val name = readSimpleIdent()
      expect(';')
      JVar(name)
    }

    private def parseTypeArgs(): List[JvmType] = {
      expect('<')
      val out = List.newBuilder[JvmType]
      while (peek != '>' && !eof) {
        out += (if (peek == '?') parseWildcard() else parseType())
      }
      expect('>')
      out.result()
    }

    private def parseWildcard(): JvmType = {
      bump() // consume '?'
      // Skip optional bound: "extends <type>" or "super <type>"
      if (i + 7 <= len && s.startsWith("extends", i)) {
        i += 7
        parseType()
      } else if (i + 5 <= len && s.startsWith("super", i)) {
        i += 5
        parseType()
      }
      JWildcard
    }

    // ── Token readers ─────────────────────────────────────────────────────────

    /** Read a class internal name (e.g. `cats/effect/IO`). Stops at `;` or `<`. */
    private def readClassName(): String = {
      val start = i
      while (i < len && s.charAt(i) != ';' && s.charAt(i) != '<') i += 1
      s.substring(start, i)
    }

    /** Read a simple identifier (type variable or type param name). */
    private def readSimpleIdent(): String = {
      val start = i
      while (
        i < len && (s.charAt(i).isLetterOrDigit || s.charAt(i) == '_' || s
          .charAt(i) == '$')
      ) i += 1
      s.substring(start, i)
    }
  }
}
