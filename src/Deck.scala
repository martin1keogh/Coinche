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

}
