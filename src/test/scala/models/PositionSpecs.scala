package mk.coinche.models

import org.scalatest._

import Position._
import Position.{North => N, West => W, South => S, East => E}

class PositionSpecs extends FunSpec {
  describe("Positions") {
    describe("#startingFrom") {
      it("should return the 4 next players in the N>W>S>E order") {
        assert(startingFrom(N).repr === N::W::S::E::Nil)
        assert(startingFrom(W).repr === W::S::E::N::Nil)
        assert(startingFrom(S).repr === S::E::N::W::Nil)
        assert(startingFrom(E).repr === E::N::W::S::Nil)
      }
    }

    describe("#after") {
      it("should return #startingFrom shifted by 1") {
        assert(after(N) === startingFrom(W))
        assert(after(W) === startingFrom(S))
        assert(after(S) === startingFrom(E))
        assert(after(E) === startingFrom(N))
      }
    }
  }
}
