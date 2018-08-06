package mk.coinche
package gamephases

import models._
import Position._

import ui._

import cats.data.StateT
import cats.implicits._
import cats.effect.IO

case class BiddingPhase(
  table: Table,
  firstBidder: Position
)(
  implicit ui: BidRead
) {
  // TODO move me? (ie generalize StateT[UserResponse]?)
  type BidState = StateT[IO, (Position, List[BidType]), List[BidType]]

  def run: IO[List[Bid]] = {
    state.iterateUntil { bids =>
      bids.size >= 4 && // give everybody a chance to bid
      bids.take(3).forall(_ == Pass)
    }
    .runA(initialState)
    .map(_.collect {
      case b: Bid => b
    })
  }

  private val initialState = (firstBidder, List[BidType]())

  // the list contains all past bids (newest first),
  private val state: BidState = StateT.apply { case (bidder, previousBids) =>
    for {
      newBid  <- readBid(bidder, previousBids.collectFirst { case Bid(_, _, v) => v })
      allBids =  newBid :: previousBids
    } yield ((after(bidder), allBids), allBids)
  }

  private def readBid(position: Position, prevValue: Option[Int]): IO[BidType] = {
    for {
      userInput <- ui.getBid(position)
      bid       =  userInput match {
                     case None => Right(Pass)
                     case Some((suit, rank)) =>
                       Bid.validate(position, suit, rank, prevValue)
                          .left.map[UserInputError](InvalidPlay(_))
                   }
      retryIf   <- bid.fold(
                     _ => /*log*/ readBid(position, prevValue),
                     valid => IO.pure(valid)
                   )
    } yield retryIf
  }
}
