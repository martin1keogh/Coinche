package mk.coinche.models

import shapeless.syntax.std.tuple._
import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class DeckSpecs extends FunSpec with Checkers {
  describe("A deck") {
    it("has only distinct cards") {
      check((d: Deck) => (d.cards diff (d.cards.toSet).toSeq).isEmpty)
    }

    it("has 32 cards") {
      assert(Deck.sorted.cards.size == 32)
    }

    describe("after shuffling") {
      it("has the same cards") {
        check((shuffled: Deck) => (shuffled.cards.toSet diff Deck.sorted.cards.toSet).isEmpty)
      }

      it("doesn't have the same order as the sorted deck") {
        check((shuffled: Deck) => (shuffled.cards != Deck.sorted.cards))
      }
    }
  }

  describe("Dealing") {
    it("doesn't duplicate cards") {
      check { (d: Deck) =>
        val hands = d.splitIntoHands.toList.flatMap(_.cards)
        hands.size == 32
        hands.distinct == hands
      }
    }

    it("deals 8 cards to each hand") {
      check((d: Deck) => d.splitIntoHands.toList.forall(_.cards.length == 8))
    }
  }
}
