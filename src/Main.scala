/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 20:23
 * To change this template use File | Settings | File Templates.
 */
object Main {

  def main(args: Array[String]) {
    Deck.init()

    for (card <- Deck.deck){
      println(card.toString)
    }

  }

}
