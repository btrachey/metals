/*
 * Vendored from VirtusLab's Inkuire (https://github.com/VirtusLab/Inkuire), v1.0.0-M9,
 * Apache License 2.0. See NOTICE.md.
 */
package scala.meta.internal.metals.signature.inkuire.service

import com.softwaremill.quicklens._
import scala.meta.internal.metals.signature.inkuire.model._

case class TypingState(
    variableBindings: VariableBindings
) {
  def addBinding(dri: ITID, typ: Type): TypingState =
    this.modify(_.variableBindings).using(_.add(dri, typ))
}

object TypingState {
  def empty: TypingState = TypingState(VariableBindings.empty)
}
