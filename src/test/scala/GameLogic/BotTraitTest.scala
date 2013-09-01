package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}
import GameLogic.Bot.DumBot
import Enchere._
import Card._

class BotTraitTest extends FlatSpec{

  val printer = new PrinterConsole
  val reader = new ReaderConsole
  val partie = new Partie(printer,reader)
  val enchereController = partie.enchereController
  val mainController = partie.mainController
  val bot = DumBot.createFromPlayer(partie,partie.j1)
  val adversaire = partie.listJoueur.find(_.equipe != bot.equipe).get

  "A Bot" should "be able to get the partner's current bid" in {
    enchereController.listEnchere = List()
    assert(bot.encherePart == None)

    enchereController.listEnchere = List(Enchere(Undef,80,adversaire.id,""))
    assert(bot.encherePart == None)

    enchereController.listEnchere = Enchere(Undef,80,bot.id,"") :: enchereController.listEnchere
    assert(bot.encherePart == None)

    val e = Enchere(Undef,80,bot.idPartenaire,"")
    enchereController.listEnchere = e :: enchereController.listEnchere
    assert(bot.encherePart equals Some(e))

    enchereController.listEnchere = List()
  }
  it should "be able to know which card is currently holding the trick" in {
    enchereController.listEnchere = List(Enchere(Pique,80,bot.id,""))
    var pli:List[Card] = List()
    assert(bot.carteMaitre(pli,None) == None)

    pli = List(Valet de Pique, Dame de Pique)
    assert(bot.carteMaitre(pli,Some(Pique)) == Some(Valet de Pique))

    pli = List(Roi de Pique, Neuf de Pique, As de Coeur)
    assert(bot.carteMaitre(pli, Some(Pique)) == Some(Neuf de Pique))

    enchereController.listEnchere = List(Enchere(Coeur,80,bot.id,""))
    pli = List(Roi de Pique, Neuf de Pique, As de Coeur)
    assert(bot.carteMaitre(pli, Some(Pique)) == Some(As de Coeur))

    enchereController.listEnchere = List(Enchere(SansAtout,100,bot.id,""))
    pli = List(As de Coeur, As de Pique, As de Carreau, As de Trefle)
    assert(bot.carteMaitre(pli, Some(Coeur)) == Some(As de Coeur))

    enchereController.listEnchere = List()
  }
  it should "be able to tell if players still have trumps" in {
    def toCards(vList:List[Valeur], c:Couleur) = vList.map(_ de c)
    enchereController.listEnchere = List(Enchere(Coeur, 100, bot.id, ""))
    mainController.cartesJoueesWithPlayer = List((bot,Valet de Coeur),(adversaire, Neuf de Coeur), (adversaire, Sept de Coeur))

    bot.main = List(As de Coeur)
    assert(bot.possedeAtout(adversaire)) // impossible de savoir s'il lui reste de l'atout, possede atout renvoi true

    bot.main = toCards(List(As,Dix,Roi,Dame,Huit), Coeur)
    assert(!bot.possedeAtout(adversaire))

    // .reverse car les carte sont structure de pile
    mainController.cartesJoueesWithPlayer = List((bot,Valet de Coeur),(adversaire, Sept de Pique)).reverse
    assert(!bot.possedeAtout(adversaire)) // il a pissé

    mainController.cartesJoueesWithPlayer = List((bot,Huit de Coeur),(adversaire, Sept de Coeur)).reverse
    assert(!bot.possedeAtout(adversaire)) // il a sous coupe a l'unique carte inferieure

    mainController.cartesJoueesWithPlayer = List((bot,Dame de Coeur),(adversaire, Sept de Coeur)).reverse
    assert(bot.possedeAtout(adversaire))

    enchereController.listEnchere = List()
  }
  it should "be able to know what the strongest trump on the table is" in {
    enchereController.listEnchere = List(Enchere(Coeur, 100, bot.id, ""))

    mainController.cartesJoueesWithPlayer = List((bot, Valet de Coeur), (adversaire, Sept de Coeur))
    assert(bot.meilleurAtoutPli(mainController.cartesJoueesWithPlayer) == Some(Valet de Coeur))

    mainController.cartesJoueesWithPlayer = List((bot, Valet de Coeur), (adversaire, Sept de Coeur)).reverse
    assert(bot.meilleurAtoutPli(mainController.cartesJoueesWithPlayer) == Some(Valet de Coeur))

    mainController.cartesJoueesWithPlayer = List((bot, Valet de Pique))
    assert(bot.meilleurAtoutPli(mainController.cartesJoueesWithPlayer) == None)

    enchereController.listEnchere = List()
  }

  it should "be able to know, given a suit, what the best card is" in {
    enchereController.listEnchere = List(Enchere(Coeur, 100, bot.id, ""))

    mainController.cartesJoueesWithPlayer = List()
    assert(bot.getValeurMaitreACouleur(Coeur) == Some(Valet))
    assert(bot.getValeurMaitreACouleur(Pique) == Some(As))

    mainController.cartesJoueesWithPlayer = List((bot,Valet de Coeur),(bot,As de Pique),(bot,Dix de Pique))
    assert(bot.getValeurMaitreACouleur(Coeur) == Some(Neuf))
    assert(bot.getValeurMaitreACouleur(Pique) == Some(Roi))


    enchereController.listEnchere = List()
  }

}
