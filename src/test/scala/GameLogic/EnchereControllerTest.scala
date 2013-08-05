package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}

/**
 * Created with IntelliJ IDeA.
 * User: martin
 * Date: 02/06/13
 * Time: 21:15
 * To change this template use File | Settings | File Templates.
 */
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
    enchereController.current = Some(new Enchere(0,100,0,"",0))
    assert(enchereController.annonceLegal(joueur,110))
    assert(!enchereController.annonceLegal(joueur,90))
    enchereController.current = None
  }
  it must "be equal to or greater than 80 if it is the first one" in {
    enchereController.current = None
    assert(enchereController.annonceLegal(joueur,80))
    assert(!enchereController.annonceLegal(joueur,70))
    assert(enchereController.annonceLegal(joueur,110))
  }
}
