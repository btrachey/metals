/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

import com.softwaremill.quicklens._

case class Signature(
    receiver: Option[Contravariance],
    arguments: Seq[Contravariance],
    result: Covariance,
    context: SignatureContext,
) {
  def typesWithVariances: Seq[Variance] = receiver.toSeq ++ arguments :+ result
  def modifyAllTypes(f: TypeLike => TypeLike): Signature =
    this
      .modifyAll(_.receiver.each.typ, _.arguments.each.typ, _.result.typ)
      .using(f)
}

object Signature {
  def apply(
      receiver: Option[TypeLike],
      arguments: Seq[TypeLike],
      result: TypeLike,
      context: SignatureContext,
  ): Signature =
    Signature(
      receiver.map(Contravariance.apply),
      arguments.map(Contravariance.apply),
      Covariance(result),
      context,
    )
}
