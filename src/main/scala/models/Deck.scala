package mk.coinche.models

case class Deck(cards: Seq[Card]) {
  def splitIntoHands(): (Hand, Hand, Hand, Hand) = {
    (cardsFor(0), cardsFor(1), cardsFor(2), cardsFor(3))
  }

  private def cardsFor(i: Int): Hand = {
    val c = takeCountFrom(i * 3, 3) ++ takeCountFrom(12 + i * 2, 2) ++ takeCountFrom(20 + i * 3, 3)
    Hand(c)
  }

  private def takeCountFrom(start: Int, count: Int): Seq[Card] = {
    cards.slice(start, start + count)
  }
}

object Deck {
  val sorted = {
    val cards = for {
      suit <- Suit.values
      rank <- Rank.values
    } yield Card(suit, rank)

    Deck(cards)
  }

  def shuffled: Deck = {
    val shuffleCards = util.Random.shuffle(sorted.cards)
    Deck(shuffleCards)
  }
}
