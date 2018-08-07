package mk.coinche
package gamephases

import models._
import Position._

import ui._

import cats.data.StateT
import cats.implicits._
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging

case class BiddingPhase(
  table: Table,
  firstBidder: Position
)(
  implicit ui: BidRead
) extends LazyLogging {
  implicit val l = logger

  def run: IO[List[Bid]] = {
    state.iterateUntil { bids =>
      bids.size >= 4 && // give everybody a chance to bid
      bids.take(3).forall(_ == Pass)
    }
    .runA(initialState)
    .map { finalBids =>
      logger.info(s"Final bid list ${finalBids.reverse}")
      finalBids.collect {
        case b: Bid => b
      }
    }
  }

  private val initialState = (firstBidder, List[BidType]())

  // the list contains all past bids (newest first),
  private val state: StateT[IO, (Position, List[BidType]), List[BidType]] = StateT.apply { case (bidder, previousBids) =>
    logger.trace(s"Going to read bid for position ${bidder}")
    for {
      newBid  <- readBid(bidder, previousBids.collectFirst { case b: Bid => b })
      _       =  logger.debug(s"Read valid bid ${newBid} from ${bidder}")
      allBids =  newBid :: previousBids
    } yield ((after(bidder), allBids), allBids)
  }

  private def readBid(position: Position, prevValue: Option[Bid]): IO[BidType] = {
    for {
      userInput <- ui.getBid(position)
      bid       =  userInput match {
                     case None => Right(Pass)
                     case Some((suit, rank)) =>
                       Bid.validate(position, suit, rank, prevValue)
                          .left.map[UserInputError](InvalidPlay(_))
                   }
      retryIf   <- bid.fold(
                     inv => {
                       logger.info(s"${bid} from ${position} is invalid, asking for a new bid")
                       readBid(position, prevValue)
                     },
                     valid => IO.pure(valid)
                   )
    } yield retryIf
  }
}
