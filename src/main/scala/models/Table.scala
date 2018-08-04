package mk.coinche.models

import Position._
import shapeless.syntax.std.tuple._

case class Table(
  north: Seat[North.type],
  west:  Seat[West.type],
  south: Seat[South.type],
  east:  Seat[East.type]
) {
  // ugh
  def dealCards(dealer: Position, deck: Deck): Table = {
    val hands = deck.splitIntoHands().toList
    (after(dealer) zip hands).foldLeft(this) {
      case (t, (North, h)) => t.copy(north = this.north.copy(hand = h))
      case (t, (West, h))  => t.copy(west  = this.west.copy(hand  = h))
      case (t, (South, h)) => t.copy(south = this.south.copy(hand = h))
      case (t, (East, h))  => t.copy(east  = this.east.copy(hand  = h))
    }
  }

  def seat[P <: Position](pos: P): Seat[P] = pos match {
    case North => north
    case West  => west
    case South => south
    case East  => east
  }
}

object Table {
  def empty(deck: Deck): Table = {
    val (n, w, s, e) = deck.splitIntoHands()
    val (ns, ws, ss, es) = (
      Seat(North, None, n),
      Seat(West,  None, w),
      Seat(South, None, s),
      Seat(East,  None, e)
    )
    Table(
      ns, ws, ss, es
    )
  }
}
