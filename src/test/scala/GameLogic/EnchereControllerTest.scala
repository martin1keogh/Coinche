package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}
import GameLogic.Enchere.Pique

class EnchereControllerTest extends FlatSpec{
  val pr = new PrinterConsole
  implicit val p = new Partie(pr,new ReaderConsole)
  val enchereController = new EnchereController
  val joueur = p.currentPlayer
 "A legal bid" must "be positive" in {
   assert(!enchereController.annonceLegal(joueur,-10))
 }
  it must "be a multiple of 10" in {
    assert(!enchereController.annonceLegal(joueur,15))
    assert(!enchereController.annonceLegal(joueur,24))
  }
  it must "be equal to or lower than 160" in {
    assert(!enchereController.annonceLegal(joueur,170))
    assert(enchereController.annonceLegal(joueur,160))
  }
  it must "be greater than the last one" in {
    enchereController.listEnchere = List(new Enchere(Pique,100,Joueur.Undef,"",Enchere.Normal))
    assert(enchereController.annonceLegal(joueur,110))
    assert(!enchereController.annonceLegal(joueur,90))
    enchereController.listEnchere = Nil
  }
  it must "be equal to or greater than 80 if it is the first one" in {
    enchereController.listEnchere = Nil
    assert(enchereController.annonceLegal(joueur,80))
    assert(!enchereController.annonceLegal(joueur,70))
    assert(enchereController.annonceLegal(joueur,110))
  }
}
