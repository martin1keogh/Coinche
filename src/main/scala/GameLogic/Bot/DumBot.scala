package GameLogic.Bot

import GameLogic.{Enchere, Joueur, Card, Partie}
import GameLogic.Enchere.{ToutAtout, SansAtout, Couleur}
import GameLogic.Card._

object DumBot {
  def createFromPlayer(partie:Partie,joueur:Joueur): DumBot = {
    val bot = new DumBot(partie,joueur.id,joueur.nom)
    bot.main = joueur.main
    bot.rename(joueur.nom+"DumBot") // default name
    bot
  }
  def createFromPlayerWithNewName(partie:Partie,joueur:Joueur,name:String) : DumBot = {
    val b = createFromPlayer(partie,joueur)
    b.rename(name)
    b
  }
}

class DumBot(val partie:Partie,id:Int,nom:String) extends Joueur(id,nom) with BotTrait{

  // Bidding //

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
    // encherePossible contient (au final) la liste des encheres a chaque couleur
    // a la fin, on retourne la `meilleur` (ie, plus elevée) de ces encheres, si elle est legal (superieur a l'enchere precedente)
    var encherePossible:List[(Couleur,Int)] = List((ToutAtout, getBidAllTrump),(SansAtout, getBidNoTrump))
    // Si le part a parlé
    if (encherePart(listEnchere).isDefined) {
      val part = encherePart(listEnchere).get
      // si on a deja parlé a cette couleur, on ne remonte pas
      if (listEnchere.exists(e => e.id == id && e.couleur == part.couleur)) ()
      else encherePossible = (part.couleur,part.contrat + remonterPart(part.couleur)) :: encherePossible
    }
    // On rajoute les encheres des couleur que l'on possede
    encherePossible = main.map(_.couleur).distinct.map(couleur => (couleur,getBid(couleur))) ::: encherePossible
    // reverse to have the bot prefer noTrump/AllTrump over low `colorBid`
    // and to stay at the partner's color
    val bestPossibleBid = encherePossible.reverse.maxBy(_._2)
    if (bestPossibleBid._2 < listEnchere.headOption.map(_.contrat).getOrElse(0)) None else Some(bestPossibleBid)
  }

  // Playing //

  // renvoie la valeur `en danger` (9 a toutAtout, les 10 a sansAtout/classique) en fonction de la couleur
  def valeurFaible(couleur:Couleur):Valeur = couleur match {
    case ToutAtout => Neuf
    case c if c == couleurAtout => Neuf
    case _ => Dix
  }

  def trouverAppel(id:Int):Option[Couleur] = {
    mainController.cartesJoueesWithPlayer.find({case (joueur,card) =>
      joueur.id == id && card.valeur != valeurFaible(card.couleur) && card.couleur != couleurAtout
    }).map(_._2.couleur)
  }

  def strategieAttaqueOuverture(jouables:List[Card],pli:List[(Joueur,Card)]):Card = {
    if (jouables.length == 1) return jouables(0)
    val adversaires = partie.listJoueur.filter(_.Equipe != Equipe)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // si un des adversaires a de l'atout, on le fait tomber
    if (adversaires.exists(possedeAtout)) {
      // si on a plus d'atout, on essaye de faire couper
      if (atout.isEmpty) {
        val couleurLaPlusTombee = mainController.cartesJouees.groupBy(_.couleur).maxBy(_._2.length)._1
        return jouables.filter(_.couleur == couleurLaPlusTombee).lastOption.getOrElse(jouables.last)
      }
      // si notre atout le plus fort est maitre, on le joue
      if (atout.contains(getCarteMaitreACouleur(couleurAtout).get)) return atout(0) // les cartes sont triees par ordre decroissant
      // sinon on joue le plus faible
      else return atout.last
    }
    // si on a une carte maitre, on la joue
    val carteMaitreOption = pasAtout.find(card => card == getCarteMaitreACouleur(card.couleur).get)
    if (carteMaitreOption.isDefined) return carteMaitreOption.get
    // si il y a eu un appel, on essaie de jouer dans cette couleur
    val appel = trouverAppel(idPartenaire)
    if (appel.isDefined) pasAtout.find(_.couleur == appel.get).getOrElse(jouables.last)
    // sinon, on joue autre chose que de l'atout, tant qu'on peut
    else pasAtout.sortBy(-_.ordreClassique).lastOption.getOrElse(atout.last)
  }

  def strategieAttaque(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Card = {
    if (jouables.length == 1) return jouables(0)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // si la couleur demande est l'atout
    if (couleurDemande == couleurAtout) {
      // si on en a
      if (!atout.isEmpty)  atout.last
      // sinon on fait un appel, si on peut
      else {
        // une couleur ou on a la carte maitre, et ou on a au moins deux cartes
        val couleurAvecCarteMaitre = pasAtout.groupBy(_.couleur).filter(_._2.exists(
          card => card == getCarteMaitreACouleur(card.couleur).get)).find(_._2.length != 1)
        if (couleurAvecCarteMaitre.isDefined) {
          val (couleur,cartes) = couleurAvecCarteMaitre.get
          if (cartes.last == valeurFaible(couleur)) cartes(0) else cartes.last
        }
        else pasAtout.last
      }
    }
    // la couleur demande n'est pas l'atout
    else {
      // si on a une carte maitre, on la joue
      val carteMaitreOption = pasAtout.find(card => card == getCarteMaitreACouleur(couleurDemande).get)
      // si ca n'a pas ete coupe
      if (carteMaitreOption.isDefined && !pli.exists(_._2.couleur == couleurAtout)) carteMaitreOption.get
      // sinon, on joue autre chose que de l'atout, tant qu'on peut
      else pasAtout.sortBy(-_.ordreClassique).lastOption.getOrElse(atout.last)
    }
  }

  def strategieDefenseOuverture(jouables:List[Card],pli:List[(Joueur,Card)]):Card = {
    if (jouables.length == 1) return jouables(0)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // si on a une carte maitre, on la joue
    val carteMaitreOption = pasAtout.find(card => card == getCarteMaitreACouleur(card.couleur).get)
    if (carteMaitreOption.isDefined) return carteMaitreOption.get
    // si il y a eu un appel, on essaie de jouer dans cette couleur
    val appel = trouverAppel(idPartenaire)
    if (appel.isDefined) pasAtout.find(_.couleur == appel.get).getOrElse(jouables.last)
    // sinon, on joue autre chose que de l'atout, tant qu'on peut
    else pasAtout.sortBy(-_.ordreClassique).lastOption.getOrElse(atout.last)
  }

  def strategieDefense(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Card = {
    if (jouables.length == 1) return jouables(0)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // si on a une carte maitre, on la joue
    val carteMaitreOption = pasAtout.find(card => card == getCarteMaitreACouleur(couleurDemande).get)
    // si ca n'a pas ete coupe
    if (carteMaitreOption.isDefined && !pli.exists(_._2.couleur == couleurAtout)) carteMaitreOption.get
    // sinon, on joue tout sauf de l'atout
    else pasAtout.sortBy(-_.ordreClassique).lastOption.getOrElse(atout.last)
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
    // si on a pris
    if (partie.enchereController.id % 2 == id % 2) {
      if (pli.isEmpty) strategieAttaqueOuverture(jouables,pli)
      else strategieAttaque(jouables,pli,couleurDemande.get)
    }
    else {
      if (pli.isEmpty) strategieDefenseOuverture(jouables,pli)
      else strategieDefense(jouables,pli,couleurDemande.get)
    }
  }
}
