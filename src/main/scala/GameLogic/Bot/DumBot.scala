package GameLogic.Bot

import GameLogic.{Enchere, Joueur, Card, Partie}
import GameLogic.Enchere.{ToutAtout, SansAtout, Couleur}
import GameLogic.Card._

class DumBot(partie:Partie,id:Int,nom:String) extends BotTrait(partie,id,nom){

  def intToValidBid(i:Int):Int = i match {
    case x if x <= 180 => x // this value is arbitrary...
    case x if x > 400 => 400
    case _ => 250
  }

  def getBid(couleur:Couleur):Int = {
    val (cartesACouleur,autres) = main.partition(_.couleur == couleur)
    val pointsAs = autres.count(_.valeur == As) * 10
    val pointsPourNombreAtouts = cartesACouleur.length match {
      case 0 => 0
      case 1 => 30
      case 2 => 50
      case 3 => 60
      case 4 => 70
      case 5 => 80
      case 6 => 100
      case 7 => 120
      case 8 => 400
    }
    val pointsPourValet = if (cartesACouleur.exists(_.valeur == Valet)) 20 else 0
    val pointsPour9Second = if (cartesACouleur.exists(_.valeur == Neuf) && cartesACouleur.length > 1) 10 else 0
    val pointsPourBelote = if (cartesACouleur.exists(_.valeur == Roi) && cartesACouleur.exists(_.valeur == Dame)) 20 else 0
    val pointsDixSec = autres.groupBy(_.couleur).count({case (_,cards) => cards.length == 1 && cards.exists(_.valeur == Dix)}) * 10
    val pointsPourCoupe = if (cartesACouleur.length>3) (3 - autres.groupBy(_.couleur).size) * 10  else 0
    val sum = pointsAs + pointsPour9Second + pointsPourValet + pointsPourNombreAtouts + pointsPourBelote - pointsDixSec + pointsPourCoupe
    if (sum >= 80) intToValidBid(sum) else 0
  }

  // Really Basic...
  def getBidNoTrump:Int = main.count(_.valeur == As) * 10 + 60 // 2As = 80, 3As = 90, ...
  def getBidAllTrump:Int = main.count(_.valeur == Valet) * 10 + 60

  def remonterPart(couleur:Couleur):Int = couleur match {
    case SansAtout => main.count(_.valeur == As) * 20
    case ToutAtout => main.count(_.valeur == Valet) * 10
    case _ => {
      val (cartesACouleur,autres) = main.partition(_.couleur == couleur)
      val pointsAs = autres.count(_.valeur == As) * 10
      val pointsPourValet = if (cartesACouleur.exists(_.valeur == Valet)) 20 else 0
      val pointsPour9Second = if (cartesACouleur.exists(_.valeur == Neuf) && cartesACouleur.length > 1) 10 else 0
      val pointsPourBelote = if (cartesACouleur.exists(_.valeur == Roi) && cartesACouleur.exists(_.valeur == Dame)) 20 else 0
      pointsAs + pointsPour9Second + pointsPourValet + pointsPourBelote
    }
  }

  /**
   *
   * @param listEnchere Liste des encheres deja annoncées
   * @return None si Passe, Some(Couleur,Contrat) sinon
   */
  def getCouleurEtContrat(listEnchere: List[Enchere]): Option[(Couleur, Int)] = {
    var encherePossible:List[(Couleur,Int)] = List((ToutAtout, getBidAllTrump),(SansAtout, getBidNoTrump))
    // Si le part a parlé
    if (encherePart(listEnchere).isDefined) {
      val part = encherePart(listEnchere).get
      // si on a deja parlé a cette couleur, on ne remonte pas
      if (listEnchere.exists(e => e.id == id && e.couleur == part.couleur)) ()
      else encherePossible = (part.couleur,part.contrat + remonterPart(part.couleur)) :: encherePossible
    }
    encherePossible = main.map(_.couleur).distinct.map(couleur => (couleur,getBid(couleur))) ::: encherePossible
    val bestPossibleBid = encherePossible.maxBy(_._2)
    if (bestPossibleBid._2 < listEnchere.headOption.map(_.contrat).getOrElse(0)) None else Some(bestPossibleBid)
  }

  /**
   *
   * @param jouables Liste des cartes jouables par le bot
   * @param autres Liste des cartes non jouables
   * @param pli Liste des cartes sur la table, dans l'ordre inverse (derniere carte jouée en premiere position)
   * @return Carte jouée par le bot
   */
  def getCard(jouables: List[Card], autres: List[Card], pli: List[(Joueur, Card)])
             (implicit couleurDemande: Option[Couleur]): Card = {
    jouables(0)
  }
}

object DumBot {
  def createFromPlayer(partie:Partie,joueur:Joueur): DumBot = {
    val bot = new DumBot(partie,joueur.id,joueur.nom)
    bot.main = joueur.main
    bot.rename(joueur.nom+"DumBot") // default name
    bot
  }
}
