package mk.coinche
package ui

import models._

import cats.effect.IO

trait BidRead {
  def getBid(position: Position): IO[Option[(BidSuit, Int)]]
}
