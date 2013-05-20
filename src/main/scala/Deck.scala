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

  val sortedDeck:List[Card] = List.tabulate(32){new Card(_)}

  def shuffle(deck:List[Card]):List[Card] = {
    Random.shuffle(deck)
  }

  def init() {
    deck = shuffle(sortedDeck)
  }

  def empty() {
    deck = List()
  }

  def distribution() = {
    val ret =
      (deck.slice(0,3)++deck.slice(12,14)++deck.slice(20,23),
       deck.slice(3,6)++deck.slice(14,16)++deck.slice(23,26),
       deck.slice(6,9)++deck.slice(16,18)++deck.slice(26,29),
       deck.slice(9,12)++deck.slice(18,20)++deck.slice(29,32)
      )
//  deck is now empty
//  It'll be filled as the game goes on
    deck = List()
    ret
  }

}
