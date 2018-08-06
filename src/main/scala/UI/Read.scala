package mk.coinche
package ui

import models._

import cats.effect.IO
import cats.implicits._
import com.typesafe.scalalogging.Logger

trait BidRead {
  protected def userInputToBid(position: Position)(implicit l: Logger): UserResponse[Option[(BidSuit, Int)]]

  def getBid(position: Position)(implicit l: Logger): IO[Option[(BidSuit, Int)]] = {
    userInputToBid(position).value.tailRecM { io =>
      io.map {
        case Left(err) =>
          l.info(s"Unable to parse bid from ${position}, got ${err}")
          Left(userInputToBid(position).value)

        case Right(bid) =>
          Right(bid)
      }
    }
  }
}
