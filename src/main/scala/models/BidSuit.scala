package mk.coinche.models

import enumeratum._

sealed trait BidSuit extends EnumEntry

object BidSuit extends Enum[BidSuit] {
  val values = findValues

  case object Hearts    extends BidSuit
  case object Spades    extends BidSuit
  case object Clubs     extends BidSuit
  case object Diamonds  extends BidSuit
  case object NoTrumps  extends BidSuit
  case object AllTrumps extends BidSuit
}
