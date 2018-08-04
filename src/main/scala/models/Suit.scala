package mk.coinche.models

import enumeratum._

sealed trait Suit extends EnumEntry

object Suit extends Enum[Suit] {
  val values = findValues

  case object Hearts   extends Suit
  case object Spades   extends Suit
  case object Clubs    extends Suit
  case object Diamonds extends Suit
}
