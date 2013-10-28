package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}
import Enchere._

class MainControllerTest extends FlatSpec{
  val Deck = new Deck
  val pr = new PrinterConsole
  val Partie = new Partie(pr, new ReaderConsole)
  val mainController = Partie.mainController

  "A complete deck" should " be worth 152 points when playing without trumps" in {
    assert (mainController.countPoints(SansAtout,Deck.newShuffledDeck) == 152)
  }
  it should "be worth 152 points when all suits are trumps" in {
    assert (mainController.countPoints(ToutAtout,Deck.newShuffledDeck) == 152)
  }
  it should "be worth 152 points when there is only one trump suit" in {
    assert (mainController.countPoints(Pique,Deck.newShuffledDeck) == 152)
  }

}
