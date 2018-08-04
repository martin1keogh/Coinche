package mk.coinche.models

case class Seat[P <: Position](
  val position: P,
  val player: Option[Player],
  val hand: Hand
)

