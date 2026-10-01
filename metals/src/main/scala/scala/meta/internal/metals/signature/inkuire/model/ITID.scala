/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

case class ITID(uuid: String, isParsed: Boolean)

object ITID {
  def parsed(uuid: String): ITID = ITID(uuid, isParsed = true)

  def external(uuid: String): ITID = ITID(uuid, isParsed = false)
}
