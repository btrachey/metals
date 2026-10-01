/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

trait SignatureFilters {
  def filterFrom(
      AnnotatedSignatures: Seq[AnnotatedSignature]
  ): Seq[AnnotatedSignature]
  def canMatch(eSgn: AnnotatedSignature): Boolean
}

object SignatureFilters {
  def include(includePackages: Seq[String]): IncludeSignatureFilters =
    new IncludeSignatureFilters(includePackages)

  def exculde(excludePackages: Seq[String]): IncludeSignatureFilters =
    new IncludeSignatureFilters(excludePackages)
}

case class IncludeSignatureFilters(includePackages: Seq[String])
    extends SignatureFilters {
  def filterFrom(
      AnnotatedSignatures: Seq[AnnotatedSignature]
  ): Seq[AnnotatedSignature] = {
    if (includePackages.isEmpty)
      AnnotatedSignatures
    else
      AnnotatedSignatures.filter(canMatch)
  }

  def canMatch(eSgn: AnnotatedSignature): Boolean =
    includePackages.exists(eSgn.packageName.contains)
}

case class ExcludeSignatureFilters(excludePackages: Seq[String])
    extends SignatureFilters {
  def filterFrom(
      AnnotatedSignatures: Seq[AnnotatedSignature]
  ): Seq[AnnotatedSignature] = {
    if (excludePackages.isEmpty)
      AnnotatedSignatures
    else
      AnnotatedSignatures.filter(canMatch)
  }

  def canMatch(eSgn: AnnotatedSignature): Boolean =
    !excludePackages.exists(eSgn.packageName.contains)
}
