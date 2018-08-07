package mk.coinche
package gamephases

import models._
import Position._
import BidSuit._
import ui._

import cats.effect.IO
import scala.collection.mutable.Queue

import org.scalatest._

class BiddingPhaseSpec extends FunSpec with Matchers {
  type I = Option[(BidSuit, Int)]

  case class BidReadFake(bids: Queue[I]) extends BidRead {
    def getBid(p: Position) = {
      IO.pure(bids.dequeueFirst(_ => true).getOrElse(None))
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

    it("accepts increasingly higher bids") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), Some(Spades -> 110), Some(Hearts -> 120), None)
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(South, H, 120), Bid(West, Spades, 110), Bid(North, H, 100)))
    }

    it("accepts capot/general bids") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), Some(Spades -> 250), Some(Hearts -> 400), None)
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(South, H, 400), Bid(West, Spades, 250), Bid(North, H, 100)))
    }

    it("accepts NoTrump/AllTrumps bids") {
      implicit val input = BidReadFake(
        Queue(Some(H -> 100), Some(NoTrumps -> 110), Some(AllTrumps -> 130), None)
      )

      val bp = BiddingPhase(Table.empty(Deck.sorted), North)
      val res = bp.run.unsafeRunSync

      res should equal (List(Bid(South, AllTrumps, 130), Bid(West, NoTrumps, 110), Bid(North, H, 100)))
    }
  }
}
