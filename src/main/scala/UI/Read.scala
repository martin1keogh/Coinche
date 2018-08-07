package mk.coinche
package ui

import models._

import cats.effect.IO

trait BidRead {
  def getPass(position: Position): IO[Pass.type]
  def getBid(position: Position): IO[(BidSuit, Int)]
  def getCoinche(bid: Bid): IO[Position]
  def getSurCoinche(bid: CoinchedBid): IO[Position]
}
