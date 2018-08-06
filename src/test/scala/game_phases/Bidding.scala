package mk.coinche
package gamephases

import models._
import Position._
import BidSuit._
import ui._

import cats.data.EitherT
import cats.effect.IO
import com.typesafe.scalalogging.Logger
import scala.collection.mutable.Queue

import org.scalatest._

class BiddingPhaseSpec extends FunSpec with Matchers {
  type I = Option[(BidSuit, Int)]

  case class BidReadFake(bids: Queue[Either[String, I]]) extends BidRead {
    def userInputToBid(p: Position)(implicit l: Logger) = EitherT.fromEither[IO] {
      val input = bids.dequeueFirst(_ => true).getOrElse(Right(None))
      input.left.map(s => ParseError(s))
    }
  }

  object BidReadFake {
    def apply(bids: Queue[I])(implicit d: DummyImplicit) = {
      new BidReadFake(bids.map(opt => Right(opt): Either[String, I]))
    }
  }

  val H: BidSuit = Hearts // help type inference

  describe("Bidding") {
    it("should stop after 3 PASS") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), None, None, None)
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(North, H, 100)))
    }

    it("should let the last player bid even after 3 PASS") {
      implicit val input = BidReadFake(
        Queue(None, None, None, Some(H -> 100))
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(East, H, 100)))
    }

    it("accepts quadruple-PASS") {
      implicit val input = BidReadFake(
        Queue[I]()
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List())
    }

    it("refuse lower bids") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), None, Some(H -> 80))
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(North, H, 100)))
    }

    it("accepts higher bids") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), Some(Spades -> 110), Some(Hearts -> 120), None)
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(South, H, 120), Bid(West, Spades, 110), Bid(North, H, 100)))
    }

    describe("in case of an invalid user input") {
      it("retries the IO call") {
        implicit val input = BidReadFake(
          Queue(Left("Err"): Either[String, I])
        )

        val bp = BiddingPhase(Table.empty(Deck.sorted), North)
        val res = bp.run.attempt.unsafeRunSync

        assert(res.isRight)
      }

      it("doesn't skip the bidder's turn") {
        implicit val input = BidReadFake(
          Queue(Left("Err"): Either[String, I], Right(Some(H -> 100)))
        )

        val bp = BiddingPhase(Table.empty(Deck.sorted), North)
        val res = bp.run.unsafeRunSync

        res should equal (List(Bid(North, H, 100)))
      }
    }
  }
}
