package scala.meta.internal.metals.signature

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * Minimal JVM class-file parser that extracts method names, descriptors,
 * generic signatures, class hierarchy, and referenced type names from
 * `.class` file bytes.
 *
 * Used to build the Inkuire signature index for dependency class files that
 * do not ship embedded SemanticDB.
 */
object ClassFileSignatures {

  // ── Public data types ───────────────────────────────────────────────────────

  /** A method's signature information from a class file. */
  final case class MethodInfo(
      name: String,
      descriptor: String,
      genericSignature: Option[String],
      isStatic: Boolean,
  )

  /** Parsed information about a single class file. */
  final case class ClassInfo(
      internalName: String,
      superClass: Option[String],
      interfaces: List[String],
      genericSignature: Option[String],
      methods: List[MethodInfo],
  )

  // ── Public API ─────────────────────────────────────────────────────────────

  /**
   * Parse method names, their descriptors and whether they are static from a
   * JVM class file.
   *
   * @return list of (methodName, descriptor, isStatic) tuples; empty list if
   *         the bytes are not a valid class file or contain no methods.
   */
  def methodDescriptors(
      classFile: Array[Byte]
  ): List[(String, String, Boolean)] =
    parse(classFile)

  /**
   * Parse a full class file, extracting hierarchy and method info including
   * the generic `Signature` attribute when present.
   *
   * @return `Some(ClassInfo)` on success, `None` if bytes are invalid
   */
  def parseClassFile(bytes: Array[Byte]): Option[ClassInfo] =
    try Some(parseFull(bytes))
    catch { case NonFatal(_) => None }

  /**
   * Extract the simple names of types referenced by a JVM method descriptor.
   *
   * Both parameter and return types are included.
   *
   * Examples:
   * {{{
   *   typeNames("(Ljava/lang/String;)I")
   *   // => Set("String", "Int")
   *
   *   typeNames("(Lcom/example/Foo;)V")
   *   // => Set("Foo")
   *
   *   typeNames("()I")
   *   // => Set("Int")
   * }}}
   */
  def typeNames(descriptor: String): Set[String] = {
    val acc = mutable.Set.empty[String]
    try parseDescriptor(descriptor, acc)
    catch { case NonFatal(_) => () }
    acc.toSet
  }

  // ---------------------------------------------------------------------------
  // Internal helpers
  // ---------------------------------------------------------------------------

  private def parse(bytes: Array[Byte]): List[(String, String, Boolean)] = {
    val out = mutable.ListBuffer.empty[(String, String, Boolean)]
    try {
      var i = 0
      def u1(): Int = { val v = bytes(i) & 0xff; i += 1; v }
      def u2(): Int = {
        val v = ((bytes(i) & 0xff) << 8) | (bytes(i + 1) & 0xff)
        i += 2
        v
      }
      def u4(): Int = { val hi = u2(); (hi << 16) | u2() }

      if (bytes.length < 8) return Nil
      if (u4() != 0xcafebabe.toInt) return Nil
      i += 4 // skip minor_version + major_version

      // Constant pool
      val cpCount = u2()
      val utf8s = mutable.Map.empty[Int, String]
      var idx = 1
      while (idx < cpCount) {
        val tag = u1()
        tag match {
          case 1 => // Utf8
            val len = u2()
            utf8s(idx) = new String(bytes, i, len, StandardCharsets.UTF_8)
            i += len
          case 5 | 6 => // Long / Double: occupy 2 slots
            i += 8
            idx += 1
          case 7 | 8 | 9 | 10 | 11 | 12 => // 2-byte payload
            i += 2
          case 15 => // InvokeDynamic: u2 + u2
            i += 4
          case 13 => // MethodHandle: u1 + u2
            i += 3
          case 4 => // Integer / Float
            i += 4
          case _ => return Nil
        }
        idx += 1
      }
      def utf8At(ci: Int): String = utf8s.getOrElse(ci, "")

      // access_flags + this_class + super_class
      i += 6
      // interfaces
      val ifCount = u2()
      i += ifCount * 2

      // Fields: skip entirely
      val fieldsCount = u2()
      var fi = 0
      while (fi < fieldsCount) {
        i += 6 // access_flags, name_index, descriptor_index
        val attrCount = u2()
        var ai = 0
        while (ai < attrCount) {
          u2() // attribute_name_index
          i += u4()
          ai += 1
        }
        fi += 1
      }

      // Methods: extract name + descriptor + static flag
      val methodsCount = u2()
      var mi = 0
      while (mi < methodsCount) {
        val accessFlags = u2()
        val nameIdx = u2()
        val descIdx = u2()
        val attrCount = u2()
        var ai = 0
        while (ai < attrCount) {
          u2() // attribute_name_index
          i += u4()
          ai += 1
        }
        val name = utf8At(nameIdx)
        val desc = utf8At(descIdx)
        val isStatic = (accessFlags & 0x0008) != 0
        // Skip static-init; keep <init> (constructor) so it shows up too.
        if (name != "<clinit>") out += ((name, desc, isStatic))
        mi += 1
      }
      out.toList
    } catch {
      case NonFatal(_) => Nil
    }
  }

  /** Full parser that extracts class hierarchy and generic signatures. */
  private def parseFull(bytes: Array[Byte]): ClassInfo = {
    var i = 0
    def u1(): Int = { val v = bytes(i) & 0xff; i += 1; v }
    def u2(): Int = {
      val v = ((bytes(i) & 0xff) << 8) | (bytes(i + 1) & 0xff)
      i += 2
      v
    }
    def u4(): Int = { val hi = u2(); (hi << 16) | u2() }

    if (bytes.length < 8) throw new Exception("too short")
    if (u4() != 0xcafebabe.toInt) throw new Exception("bad magic")
    i += 4 // skip minor_version + major_version

    // Constant pool
    val cpCount = u2()
    val utf8s = mutable.Map.empty[Int, String]
    var idx = 1
    while (idx < cpCount) {
      val tag = u1()
      tag match {
        case 1 => // Utf8
          val len = u2()
          utf8s(idx) = new String(bytes, i, len, StandardCharsets.UTF_8)
          i += len
        case 5 | 6 =>
          i += 8
          idx += 1
        case 7 | 8 | 9 | 10 | 11 | 12 =>
          i += 2
        case 15 =>
          i += 4
        case 13 =>
          i += 3
        case 4 =>
          i += 4
        case _ =>
          throw new Exception(s"unknown constant pool tag $tag")
      }
      idx += 1
    }
    def utf8At(ci: Int): String = utf8s.getOrElse(ci, "")

    // access_flags(2) + this_class(2) + super_class(2)
    i += 2 // access_flags
    val thisClassIdx = u2()
    val superClassIdx = u2()

    // interfaces
    val ifCount = u2()
    val interfaces = (1 to ifCount).map { _ =>
      val idx2 = u2()
      utf8At(idx2)
    }.toList

    // Fields: skip entirely
    val fieldsCount = u2()
    var fi = 0
    while (fi < fieldsCount) {
      i += 6
      val attrCount = u2()
      var ai = 0
      while (ai < attrCount) {
        u2()
        i += u4()
        ai += 1
      }
      fi += 1
    }

    // Methods: extract name + descriptor + generic signature
    val methodsCount = u2()
    val methods = mutable.ListBuffer.empty[MethodInfo]
    var mi = 0
    while (mi < methodsCount) {
      val accessFlags = u2()
      val nameIdx = u2()
      val descIdx = u2()
      val attrCount = u2()
      var ai = 0
      var genericSig: Option[String] = None
      while (ai < attrCount) {
        val attrNameIdx = u2()
        val attrLen = u4()
        val attrName = utf8At(attrNameIdx)
        if (attrName == "Signature") {
          val sigIdx = u2()
          genericSig = Some(utf8At(sigIdx))
          i += attrLen - 2
        } else {
          i += attrLen
        }
        ai += 1
      }
      val name = utf8At(nameIdx)
      val desc = utf8At(descIdx)
      val isStatic = (accessFlags & 0x0008) != 0
      if (name != "<clinit>")
        methods += MethodInfo(name, desc, genericSig, isStatic)
      mi += 1
    }

    // Class-level attributes (after methods)
    val classAttrCount = u2()
    var classGenericSig: Option[String] = None
    var cai = 0
    while (cai < classAttrCount) {
      val attrNameIdx = u2()
      val attrLen = u4()
      val attrName = utf8At(attrNameIdx)
      if (attrName == "Signature") {
        val sigIdx = u2()
        classGenericSig = Some(utf8At(sigIdx))
        i += attrLen - 2
      } else {
        i += attrLen
      }
      cai += 1
    }

    val thisClassName = utf8At(thisClassIdx)
    val superClass =
      if (superClassIdx == 0) None
      else Some(utf8At(superClassIdx))

    ClassInfo(
      internalName = thisClassName,
      superClass = superClass,
      interfaces = interfaces,
      genericSignature = classGenericSig,
      methods = methods.toList,
    )
  }

  private def parseDescriptor(
      desc: String,
      acc: mutable.Set[String],
  ): Unit = {
    var i = 0
    if (i < desc.length && desc.charAt(i) == '(') {
      i += 1
      while (i < desc.length && desc.charAt(i) != ')') {
        i = skipType(desc, i, acc)
      }
      if (i < desc.length && desc.charAt(i) == ')') i += 1
      if (i < desc.length && desc.charAt(i) != 'V') {
        skipType(desc, i, acc)
      }
    }
  }

  /**
   * Parse one type starting at position `i` in `desc`.
   * Returns the index immediately after the type.
   */
  private def skipType(
      desc: String,
      i: Int,
      acc: mutable.Set[String],
  ): Int = {
    desc.charAt(i) match {
      case '[' =>
        var j = i
        while (j < desc.length && desc.charAt(j) == '[') j += 1
        skipType(desc, j, acc)
      case 'L' =>
        val end = desc.indexOf(';', i + 1)
        if (end > i) {
          val internal = desc.substring(i + 1, end)
          addClassName(internal, acc)
          end + 1
        } else desc.length
      case other =>
        addPrimitive(other, acc)
        i + 1
    }
  }

  private def addClassName(internal: String, acc: mutable.Set[String]): Unit = {
    val simple = internal.split('/').last
    if (simple.nonEmpty) acc += simple
  }

  private val PrimitiveNames: Map[Char, String] = Map(
    'I' -> "Int",
    'J' -> "Long",
    'Z' -> "Boolean",
    'D' -> "Double",
    'F' -> "Float",
    'C' -> "Char",
    'B' -> "Byte",
    'S' -> "Short",
  )

  private def addPrimitive(c: Char, acc: mutable.Set[String]): Unit =
    PrimitiveNames.get(c).foreach(acc += _)
}
