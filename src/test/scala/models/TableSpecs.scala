package mk.coinche.models

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.ScalacheckShapeless._

import Rank._, Suit._

class TableSpecs extends FunSpec with Matchers with Checkers {
  describe("An empty table") {
    it("should only have empty players") {
      val d = Deck.sorted
      val t = Table.empty(d)
      t.north.player shouldBe empty
      t.south.player shouldBe empty
      t.west.player  shouldBe empty
      t.east.player  shouldBe empty
    }

    it("should cards in each hands") {
      val d = Deck.sorted
      val t = Table.empty(d)
      t.north.hand.cards.length shouldBe (8)
      t.south.hand.cards.length shouldBe (8)
      t.west.hand.cards.length  shouldBe (8)
      t.east.hand.cards.length  shouldBe (8)
    }
  }

  describe("Dealing cards") {
    // check we don't start dealing cards to the dealer first
    it("should give the 3 first cards to the player after the dealer") {
      val d = Deck.sorted
      val t = Table.empty(d)
      check { (p: Position) =>
        val dealer = p
        val newTable = t.dealCards(dealer = dealer, deck = d)
        val afterDealer = Position.after(dealer)

        newTable.seat(afterDealer).hand.cards.take(3) == (List(
          Seven of Hearts, Eight of Hearts, Nine of Hearts
        ))
      }
    }

    it("should deal 8 cards to each players") {
      check { (p: Position, d: Deck) =>
        val t = Table.empty(d).dealCards(p, d)
        Position.values.forall { pos =>
          t.seat(pos).hand.cards.length == 8
        }
      }
    }
  }
}
