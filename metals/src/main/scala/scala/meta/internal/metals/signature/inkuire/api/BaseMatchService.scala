/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.api

import scala.meta.internal.metals.signature.inkuire.api.InkuireDb
import scala.meta.internal.metals.signature.inkuire.model.AnnotatedSignature
import scala.meta.internal.metals.signature.inkuire.model.ResolveResult
import scala.meta.internal.metals.signature.inkuire.model.Signature

trait BaseMatchService {
  def inkuireDb: InkuireDb
  def findMatches(
      resolveResult: ResolveResult
  ): Seq[(AnnotatedSignature, Signature)]
  def isMatch(resolveResult: ResolveResult)(
      against: AnnotatedSignature
  ): Option[Signature]
}
