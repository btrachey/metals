/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.model

import scala.language.implicitConversions

case class TypeName(name: String) {
  override def hashCode(): Int = name.toLowerCase.hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: TypeName => this.name.toLowerCase == o.name.toLowerCase
      case _ => false
    }
  }

  override def toString: String = name
}

object TypeName {
  implicit def stringToTypeName(str: String): TypeName = TypeName(str)
}
