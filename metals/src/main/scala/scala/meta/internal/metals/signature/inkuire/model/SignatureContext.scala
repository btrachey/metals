/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

import scala.meta.internal.metals.signature.inkuire.utils.Monoid

case class SignatureContext(
    vars: Set[String],
    constraints: Map[String, Seq[TypeLike]],
) {
  override def equals(obj: Any): Boolean =
    obj match {
      case other: SignatureContext if this.vars.size == other.vars.size => true
      case _ => false
    }
}

object SignatureContext {

  def empty: SignatureContext = Monoid[SignatureContext].empty

  implicit val signatureContextMonoid: Monoid[SignatureContext] =
    new Monoid[SignatureContext] {
      override def empty: SignatureContext =
        SignatureContext(Set.empty, Map.empty)

      override def mappend(
          x: SignatureContext,
          y: SignatureContext,
      ): SignatureContext = {
        SignatureContext(
          x.vars ++ y.vars,
          x.constraints ++ y.constraints,
        )
      }
    }
}
