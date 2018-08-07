package mk.coinche.models

sealed trait BidType {
  def value: Int
}

case object Pass extends BidType { val value = 0 }
case class Bid(position: Position, suit: BidSuit, value: Int) extends BidType

object Bid {
  // can probably do better than the string, but meh
  def validate(position: Position, suit: BidSuit, value: Int, prev: Option[Bid]): Either[String, Bid] = {
    if (value <= prev.fold(0)(_.value)) Left("Bid value must be greater than the latest one!")
    else if (value % 10 != 0) Left("Bid value must be a multiple of 10!")
    else if (value < 80) Left("Bid value must be greater than 80")
    else if (value > 180 && (value != 250 && value != 400)) Left("Non capot/general bids cannot exceed 180")
    else Right(Bid(position, suit, value))
  }
}
