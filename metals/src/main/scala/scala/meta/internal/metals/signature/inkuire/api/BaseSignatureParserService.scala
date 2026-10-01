/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.api

import scala.meta.internal.metals.signature.inkuire.model.ParsedSignature

trait BaseSignatureParserService {
  def parse(str: String): Either[String, ParsedSignature]
}
