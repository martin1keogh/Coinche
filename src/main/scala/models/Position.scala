package mk.coinche.models

import enumeratum._

sealed trait Position extends EnumEntry

object Position extends Enum[Position] {
  val values = findValues

  case object North extends Position
  case object West  extends Position
  case object South extends Position
  case object East  extends Position
}
