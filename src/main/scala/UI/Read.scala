package mk.coinche
package ui

import models._

import cats.effect.IO
import cats.implicits._

trait BidRead {
  protected def userInputToBid(position: Position): UserResponse[Option[(BidSuit, Int)]]

  def getBid(position: Position): IO[Option[(BidSuit, Int)]] = {
    userInputToBid(position).value.tailRecM { io =>
      io.map {
        case Left(/*TODO log inputErr*/ _) => Left(userInputToBid(position).value)
        case Right(bid)     => Right(bid)
      }
    }
  }
}
