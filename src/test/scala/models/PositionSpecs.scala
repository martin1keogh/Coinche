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
      it("should return the next player") {
        assert(after(N) === W)
        assert(after(W) === S)
        assert(after(S) === E)
        assert(after(E) === N)
      }
    }
  }

  describe("Teams") {
    it("splits the positions into NS vs WE") {
      assert(N sameTeamAs S)
      assert(S sameTeamAs N)
      assert(W sameTeamAs E)
      assert(E sameTeamAs W)

      assert(!(N sameTeamAs W))
      assert(!(N sameTeamAs E))
      assert(!(S sameTeamAs W))
      assert(!(S sameTeamAs E))

      assert(!(W sameTeamAs N))
      assert(!(W sameTeamAs N))
      assert(!(E sameTeamAs S))
      assert(!(E sameTeamAs S))
    }
  }
}
