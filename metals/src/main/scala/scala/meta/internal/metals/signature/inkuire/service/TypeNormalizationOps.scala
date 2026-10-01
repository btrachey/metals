/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.service

import scala.meta.internal.metals.signature.inkuire.model._
import com.softwaremill.quicklens._

trait TypeNormalizationOps {
  def uncurryTypes(tpe: TypeLike): TypeLike = tpe match {
    case TypeLambda(args, t: Type)
        if args.zip(t.params).forall { case (a, p) => a == p.typ } =>
      uncurryTypes(t.modify(_.params).setTo(List.empty))
    case t: Type =>
      t.modify(_.params.each.typ).using(uncurryTypes)
    case t: OrType =>
      t.modifyAll(_.left, _.right).using(uncurryTypes)
    case t: AndType =>
      t.modifyAll(_.left, _.right).using(uncurryTypes)
    case t: TypeLambda =>
      t.modify(_.result).using(uncurryTypes)
  }

  def uncurrySignature(sgn: Signature): Signature =
    sgn.modifyAllTypes(uncurryTypes)
}
