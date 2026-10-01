/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.service

import scala.meta.internal.metals.signature.inkuire.model.Type
import scala.meta.internal.metals.signature.inkuire.model._

import scala.util.parsing.combinator.RegexParsers

abstract class BaseSignatureParser extends RegexParsers {

  def identifier: Parser[String] = """[A-Za-z]\w*""".r

  def nullability: Parser[Boolean] = "?" ^^^ true | "" ^^^ false

  def list[A](typ: Parser[A]): Parser[Seq[A]] =
    (typ <~ ",") ~ list(typ) ^^ { case head ~ tail => head +: tail } |
      typ ^^ (Seq(_))

  def empty[A]: Parser[List[A]] = "" ^^^ List.empty

  def genericType: Parser[Type]

  def functionType: Parser[Type]

  def signature: Parser[ParsedSignature]

}
