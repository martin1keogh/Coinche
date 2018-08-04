package mk.coinche

import org.scalacheck._

package object models {
  implicit val deckGen: Gen[Deck] = Arbitrary.arbLong.arbitrary.map { l =>
    scala.util.Random.setSeed(l)
    Deck.shuffled
  }

  implicit val deckArb: Arbitrary[Deck] = Arbitrary(deckGen)
}
