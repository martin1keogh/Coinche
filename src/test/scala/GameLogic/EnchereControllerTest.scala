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
  implicit val p = new Partie(pr,new ReaderConsole(pr))
  val enchereController = new EnchereController
 "A legal bid" must "be positive" in {
   assert(!enchereController.annonceLegal(-10))
 }
  it must "be a multiple of 10" in {
    assert(!enchereController.annonceLegal(15))
    assert(!enchereController.annonceLegal(24))
  }
  it must "be equal to or lower than 160" in {
    assert(!enchereController.annonceLegal(170))
    assert(enchereController.annonceLegal(160))
  }
  it must "be greater than the last one" in {
    enchereController.current = Some(new Enchere(0,100,0,"",0))
    assert(enchereController.annonceLegal(110))
    assert(!enchereController.annonceLegal(90))
    enchereController.current = None
  }
  it must "be equal to or greater than 80 if it is the first one" in {
    enchereController.current = None
    assert(enchereController.annonceLegal(80))
    assert(!enchereController.annonceLegal(70))
    assert(enchereController.annonceLegal(110))
  }
  "To still be able to bid, the last bid" must "be lower than 400" in {
    enchereController.current = Some(new Enchere(0,400,0,"",0))
    assert(enchereController.annonceImpossible())
    enchereController.current = None
  }
  it must "not have been 'coinché'" in {
    enchereController.current = Some(new Enchere(0,130,0,"",2))
    assert(enchereController.annonceImpossible())
    enchereController.current = None
  }
}
