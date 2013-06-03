package GameLogic

import org.scalatest.FlatSpec
import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 14:27
 * To change this template use File | Settings | File Templates.
 */
class DeckTest extends FlatSpec{
  "A sorted deck" must "have exactly 32 cards" in{
    assert(Deck.sortedDeck.size == 32)
  }
  it must "have only distinct cards" in{
    assert(Deck.sortedDeck.size == Deck.sortedDeck.distinct.size)
  }
  "A shuffled deck" must "have exactly 32 cards" in{
    val deck = Deck.newShuffledDeck
    assert(deck.size == 32)
  }
  it must "have only distinct cards" in {
    val deck = Deck.newShuffledDeck
    assert (deck.size == deck.distinct.size)
  }
  "After distribtion, the players" should "have 8 cards each" in {
    val deck = Deck.newShuffledDeck
    val l = Deck.distribution(deck)
    assert (l.forall(_.size == 8))
  }
  it should "have only distinct cards" in {
    val deck = Deck.newShuffledDeck
    val l = Deck.distribution(deck)
    assert(l.flatten.distinct.size == (l.map(_.size).sum))
  }
  "After cutting a deck, it" should "still have the same number of cards" in {
    val deck = Deck.newShuffledDeck
    assert(Deck.coupe(deck,Random.nextInt(25)+4).get.size == deck.size)
  }
}
