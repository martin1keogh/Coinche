package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}
import GameLogic.Enchere.{Coinche, Normal, ToutAtout, Pique}

class EnchereControllerTest extends FlatSpec{
  val pr = new PrinterConsole
  implicit val p = new Partie(pr,new ReaderConsole)
  val enchereController = new EnchereController
  val joueur = p.currentPlayer
  val partner = p.listJoueur.find(_.id == joueur.idPartenaire).get
  val adversaire = p.listJoueur.find(_ == p.nextPlayer(joueur)).get
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
  "A player" should "not be able to 'coinche' if  he or his partner are currently winning the bids" in {
    enchereController.listEnchere = List(Enchere(Pique,90,joueur.id,joueur.nom))
    assert(!enchereController.coincheValid(joueur))
    assert(!enchereController.coincheValid(partner))
  }
  it should "not be able to coinche a '80" in {
    enchereController.listEnchere = List(Enchere(ToutAtout,80,adversaire.id,adversaire.nom))
    assert(!enchereController.coincheValid(joueur))
  }
  it should "not be able to 'surcoinche a bid that hasn't been coinche" in {
    enchereController.listEnchere = List(Enchere(Pique,100,joueur.id,joueur.nom,Normal))
    assert(!enchereController.surCoincheValid(joueur))
  }
  it should "not be able to surcoinche a bid that was made by the other team" in {
    enchereController.listEnchere = List(Enchere(Pique,100,adversaire.id,adversaire.nom,Coinche))
    assert(!enchereController.surCoincheValid(joueur))
    assert(!enchereController.surCoincheValid(partner))
  }
  it should "be able to coinche if the last bid was by the other team, hasn't been coinche yet and wasn't a 80" in {
    enchereController.listEnchere = List(Enchere(Pique,110, adversaire.id,adversaire.nom, Normal))
    assert(enchereController.coincheValid(joueur))
    assert(enchereController.coincheValid(partner))
  }
  it should "be able to coinche a bid by his own team which was coinche" in {
    enchereController.listEnchere = List(Enchere(Pique,100, partner.id,partner.nom, Coinche))
    assert(enchereController.surCoincheValid(partner))
    assert(enchereController.surCoincheValid(joueur))
  }
}
