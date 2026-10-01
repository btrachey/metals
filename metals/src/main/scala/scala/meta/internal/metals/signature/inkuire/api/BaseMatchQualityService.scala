/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.api

import scala.meta.internal.metals.signature.inkuire.model._

trait BaseMatchQualityService {
  def sortMatches(
      functions: Seq[(AnnotatedSignature, Signature)]
  ): Seq[(AnnotatedSignature, Int)] =
    functions
      .map { case (fun, matching) =>
        fun -> matchQualityMetric(fun, matching)
      }
      .sortBy(_._2)

  def matchQualityMetric(
      AnnotatedSignature: AnnotatedSignature,
      matching: Signature,
  ): Int
}
