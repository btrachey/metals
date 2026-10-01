/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.utils

trait Monoid[A] {
  def empty: A
  def mappend(a: A, b: A): A
}

object Monoid {

  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid

  def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldLeft[A](monoid.empty) { case (acc, a) =>
      monoid.mappend(acc, a)
    }

  implicit class MonoidOps[A](a: A) {
    def <>(a1: A)(implicit monoid: Monoid[A]): A =
      monoid.mappend(a, a1)
  }
}
