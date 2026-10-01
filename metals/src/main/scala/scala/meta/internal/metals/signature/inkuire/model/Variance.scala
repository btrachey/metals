/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

sealed trait Variance {
  val typ: TypeLike
}

/**
 * Java Klass<? extends Param>
 * Kotlin Klass<out Param>
 * Scala Klass[+Param]
 */
case class Covariance(typ: TypeLike) extends Variance

/**
 * Java Klass<? super Param>
 * Kotlin Klass<in Param>
 * Scala Klass[-Param]
 */
case class Contravariance(typ: TypeLike) extends Variance

/**
 * Java Klass<Param>
 * Kotlin Klass<Param>
 * Scala Klass[Param]
 */
case class Invariance(typ: TypeLike) extends Variance

/**
 * Variance of `types` from queries
 */
case class UnresolvedVariance(typ: TypeLike) extends Variance
