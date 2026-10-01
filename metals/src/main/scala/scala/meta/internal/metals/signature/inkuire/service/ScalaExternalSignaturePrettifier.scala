/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.service

import scala.meta.internal.metals.signature.inkuire.api._
import scala.meta.internal.metals.signature.inkuire.model._

class ScalaAnnotatedSignaturePrettifier extends BaseSignaturePrettifier {

  override def prettifyAll(sgns: Seq[AnnotatedSignature]): String =
    sgns.map(prettify).mkString("\n")

  override def prettify(esgn: AnnotatedSignature): String =
    s"${prettifySignature(esgn.signature)}"

  def prettifySignature(sgn: Signature): String = {
    s"${prettifyTypeVariables(sgn.context)}" +
      s"${prettifyArgs(sgn.typesWithVariances, " => ")}"
  }

  private def prettifyTypeVariables(context: SignatureContext): String = {
    if (context.vars.isEmpty) ""
    else {
      s"[${context.vars.mkString(", ")}] => "
    }
  }

  private def prettifyArgs(args: Seq[Variance], sep: String = ", "): String =
    args.map(_.typ).map(prettifyType).mkString(sep)

  def prettifyType(t: TypeLike): String = t match {
    case t: Type if t.isStarProjection => "?"
    case t: Type
        if t.isGeneric && !t.isVariable && t.name.name.matches("Function.*") =>
      s"(${prettifyArgs(t.params, " => ")})"
    case t: Type
        if t.isGeneric && !t.isVariable && t.name.name.matches("Tuple.*") =>
      s"(${prettifyArgs(t.params)})"
    case t: Type if t.isGeneric =>
      s"${t.name}[${prettifyArgs(t.params)}]"
    case t: Type => s"${t.name}"
    case AndType(left, right) =>
      "(" + prettifyType(left) + " & " + prettifyType(right) + ")"
    case OrType(left, right) =>
      "(" + prettifyType(left) + " | " + prettifyType(right) + ")"
    case TypeLambda(Seq(arg), res: Type)
        if res.params.size == 1 && res.params.head.typ.isInstanceOf[Type] &&
          res.params.head.typ.asInstanceOf[Type].itid == arg.itid =>
      s"${res.name}[_]"
    case TypeLambda(args, res) =>
      "[" + args.map(_.name.name).mkString(", ") + "] =>> " + prettifyType(res)
  }
}
