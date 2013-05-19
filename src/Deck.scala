import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 19:51
 * To change this template use File | Settings | File Templates.
 */
object Deck {
  var deck:List[Card]=List()

  def sortedDeck:List[Card] = {
    var tmpDeck:List[Card] = List()
    for (i <- 0 to 51) {
      tmpDeck = (new Card(i))::tmpDeck
    }
    tmpDeck
  }

  def shuffle(deck:List[Card]):List[Card] = {
    Random.shuffle(deck)
  }

  def init() {
    deck = shuffle(sortedDeck)
  }

  def distribution() = {
    val ret =
      (deck.slice(0,3)++deck.slice(12,14)++deck.slice(20,23),
       deck.slice(3,6)++deck.slice(14,16)++deck.slice(23,26),
       deck.slice(6,9)++deck.slice(16,18)++deck.slice(26,29),
       deck.slice(9,12)++deck.slice(18,20)++deck.slice(29,32)
      )
//  Deck is now empty
//  It'll be filled as the game goes on
    deck = List()
    ret
  }

}
