/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

case class AnnotatedSignature(
    signature: Signature,
    name: String,
    packageName: String,
    uri: String,
    entryType: String,
) {
  val uuid: String = entryType + packageName + name + uri
}
