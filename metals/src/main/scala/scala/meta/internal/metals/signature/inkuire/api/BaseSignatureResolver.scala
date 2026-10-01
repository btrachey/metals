/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.api

import scala.meta.internal.metals.signature.inkuire.model.ParsedSignature
import scala.meta.internal.metals.signature.inkuire.model.ResolveResult

trait BaseSignatureResolver {
  def resolve(parsed: ParsedSignature): Either[String, ResolveResult]
}
