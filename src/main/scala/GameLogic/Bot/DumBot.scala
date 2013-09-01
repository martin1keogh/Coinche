package GameLogic.Bot

import GameLogic.{Enchere, Joueur, Card, Partie}
import GameLogic.Enchere.{Undef, ToutAtout, SansAtout, Couleur}
import GameLogic.Card._
import GameLogic.Joueur.Position

object DumBot {
  def createFromPlayer(partie:Partie,joueur:Joueur): DumBot = {
    val bot = new DumBot(partie,joueur.id,joueur.nom)
    bot.main = joueur.main
    bot.rename(joueur.nom+"DumBot") // default name
    bot
  }
  def createFromPlayer(partie:Partie,joueur:Joueur,name:String) : DumBot = {
    val b = createFromPlayer(partie,joueur)
    b.rename(name)
    b
  }
}

class DumBot(val partie:Partie,id:Position,nom:String) extends Joueur(id,nom) with BotTrait{

  // Bidding //

  def intToValidBid(i:Int):Int = i match {
    case x if x < 80 => 0
    case x if x <= 180 => x.min(160) // this value is arbitrary...
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
    intToValidBid(sum)
  }

  // Really Basic...
  def getBidNoTrump:Int = main.count(_.valeur == As) * 10 + 60 // 2As = 80, 3As = 90, ...
  def getBidAllTrump:Int = main.count(_.valeur == Valet) * 10 + 60

  def remonterPart(couleur:Couleur):Int = couleur match {
    case SansAtout => main.count(_.valeur == As) * 20
    case ToutAtout => main.count(_.valeur == Valet) * 10
    case _ => {
      val (cartesACouleur,autres) = main.partition(_.couleur == couleur)
      // on ne remonte pas si on a parle a Sans atout avant
      val pointsAs = if (partie.enchereController.listEnchere.exists(e => e.id == id && e.couleur == SansAtout)) 0
                     else autres.count(_.valeur == As) * 10
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

  /**
   *
   * @param cards la liste des cartes parmi lesquels chercher une carte maitre
   * @param couleurDemande la couleur a laquelle chercher une carte maitre
   * @return Some(card) si card est la carte maitre a `couleurDemande` ET si le pli n'a pas ete coupe,
   *         None sinon
   */
  def getCarteMaitreOption(pli:List[(Joueur,Card)],cards:List[Card],couleurDemande:Couleur):Option[Card] = {
    if (pli.exists(_._2.couleur == couleurAtout) && couleurDemande != couleurAtout) None
    else {
      val valeurMaitre = getValeurMaitreACouleur(couleurDemande)
      if (valeurMaitre.isDefined) cards.filter(_.couleur == couleurDemande).find(_.valeur == valeurMaitre.get)
      else None
    }
  }

  /**
   *
   * @param cards
   * @return Some(card) si card est la carte maitre a sa couleur, None si aucune card n'est maitre a sa couleur
   */
  def getCarteMaitreOption(cards:List[Card]):Option[Card] =
    cards.find(card => card.valeur == getValeurMaitreACouleur(card.couleur).get)

  def trouverAppel:Option[Couleur] = {
    val listePlis = mainController.cartesJoueesWithPlayer.grouped(4).toList
    listePlis.find({case l:List[(Joueur,Card)] => {
      l.find(_._1.id == idPartenaire).exists({case (_,card) =>
        card.valeur != valeurFaible(card.couleur) && card.couleur != couleurAtout && card.couleur != l.last._2.couleur
      })
    }}).getOrElse(List[(Joueur,Card)]()).find(_._1.id == idPartenaire).map(_._2.couleur)
  }

  def repondreAppel(cards:List[Card]):Option[Card] = {
    val appel = trouverAppel
    if (appel.isEmpty) None
    else cards.filter(_.couleur == appel.get).lastOption
  }

  def lancerAppel(pli:List[(Joueur,Card)],pasAtout:List[Card],couleurDemande:Couleur):Option[Card] = {
    // une couleur ou on a la carte maitre, et ou on a au moins deux cartes
    val couleurAvecCarteMaitre = pasAtout.groupBy(_.couleur).filter(_._2.exists(card =>
      card.valeur == getValeurMaitreACouleur(card.couleur).get)).find(_._2.length > 1)
    if (couleurAvecCarteMaitre.isDefined && partGagnePliSaufCoupe(pli,couleurDemande)) {
      val (couleur,cartes) = couleurAvecCarteMaitre.get
      // ne pas lancer le dix si on a As-Dix...
      if (cartes.last.valeur == valeurFaible(couleur)) cartes.headOption else cartes.lastOption
    }
    else None
  }

  // si le joueur ou le part est SUR d'etre maitre, sauve les 10 secs
  // essaie aussi de sauver le neuf sec d'atout, etc...
  def sauverPoints(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Option[Card] = {
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // Si on a le neuf, l'as ou le dix d'atout, sec, et que le valet n'est pas tombe
    if (atout.length == 1 && atout.exists(c => (c.valeur == Neuf || c.valeur == Dix || c.valeur == As) &&
        c.valeur != getValeurMaitreACouleur(couleurAtout).get)) atout.headOption
    else {
      val sansCartesMaitres = pasAtout.filterNot(c => c.valeur == getValeurMaitreACouleurApresPli(c.couleur,pli.map(_._2)).get)
      // on pisse si le part est maitre
      if (partGagnePliSaufCoupe(pli,couleurDemande))
        sansCartesMaitres.find(card => card == valeurFaible(card.couleur) && pasAtout.count(c => c.couleur == card.couleur) == 1)
      else if (pli.length == 3) { //si on est le dernier a jouer
        pasAtout.find(card => card == carteMaitre.get)
      } else None
    }
  }

  def pisser(pasAtout:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Option[Card] = {
    val sansCartesMaitre = pasAtout.filterNot(c => c.valeur == getValeurMaitreACouleurApresPli(c.couleur,pli.map(_._2)).get)
    val trieParPointCroissant = sansCartesMaitre.sortBy(_.ordreClassique)
    val sansMaitre = if (partGagnePliSaufCoupe(pli,couleurDemande)) trieParPointCroissant.lastOption
    else trieParPointCroissant.headOption
    // on essaie d'abord de pisser des cartes qui ne sont pas maitres
    sansMaitre orElse pasAtout.lastOption
  }

  // renvoie la Couleur la plus jouée pour l'instant, ou None si aucune carte n'a ete jouee
  def couleurLaPlusJoueOption:Option[Couleur] = {
    if (mainController.cartesJouees.isEmpty) None
    else Some(mainController.cartesJouees.groupBy(_.couleur).maxBy(_._2.length)._1)
  }

  def strategieAttaqueOuverture(jouables:List[Card],pli:List[(Joueur,Card)]):Card = {
    val adversaires = partie.listJoueur.filter(_.equipe != equipe)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // si un des adversaires a de l'atout, on le fait tomber
    if (adversaires.exists(possedeAtout)) {
      (getCarteMaitreOption(atout) orElse  // on joue l'atout maitre
        atout.lastOption orElse // ou alors son atout le plus faible
        pasAtout.filter(_.couleur == couleurLaPlusJoueOption.getOrElse(Undef)).lastOption orElse  // ou la couleur la plus joue
        pasAtout.sortBy(_.pointsClassique).headOption).get
    }
    else
      (getCarteMaitreOption(pasAtout) orElse // on joue ses plis maitres
        repondreAppel(pasAtout) orElse // on essaie de repondre a un appel
        pasAtout.sortBy(_.pointsClassique).headOption orElse // sinon , on joue en priorite les cartes autres que l'atout
        atout.lastOption).get
  }

  def strategieAttaque(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Card = {
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    (getCarteMaitreOption(pli,jouables,couleurDemande) orElse // on joue la carte maitre a la couleur demande
      sauverPoints(jouables,pli,couleurDemande) orElse // sinon on pisse des points o/
      lancerAppel(pli,pasAtout,couleurDemande) orElse // on fait un appel
      pisser(pasAtout,pli,couleurDemande) orElse
      atout.find(c => c != getValeurMaitreACouleurApresPli(couleurAtout, pli.map(_._2)))
      ).get
  }

  def strategiePartGeneral(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Card = {
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    (atout.headOption orElse pasAtout.sortBy(_.ordreClassique).lastOption).get
  }

  def strategieDefenseOuverture(jouables:List[Card],pli:List[(Joueur,Card)]):Card = {
    val adversaires = partie.listJoueur.filter(_.equipe != equipe)
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    // on essaie d'abord de faire couper
    if (adversaires.exists(possedeAtout)) {
      (pasAtout.filter(_.couleur == couleurLaPlusJoueOption.getOrElse(Undef)).lastOption orElse // on joue la couleur la plus joue
        getCarteMaitreOption(pasAtout) orElse
        pasAtout.sortBy(_.pointsClassique).headOption orElse
        atout.lastOption).get
    }
    else
      (getCarteMaitreOption(pasAtout) orElse // on joue ses plis maitres
        repondreAppel(pasAtout) orElse // on essaie de repondre a un appel
        pasAtout.sortBy(_.pointsClassique).headOption orElse // sinon , on joue en priorite les cartes autres que l'atout
        atout.lastOption).get
  }

  def strategieDefense(jouables:List[Card],pli:List[(Joueur,Card)],couleurDemande:Couleur):Card = {
    val (atout,pasAtout) = jouables.partition(_.couleur == couleurAtout)
    (getCarteMaitreOption(pli,jouables,couleurDemande) orElse // on joue la carte maitre a la couleur demande
      sauverPoints(jouables,pli,couleurDemande) orElse // sinon on pisse des points o/
      lancerAppel(pli,pasAtout,couleurDemande) orElse // on fait un appel
      pisser(pasAtout,pli,couleurDemande) orElse
      atout.lastOption).get // sinon on coupe avec son atout le plus faible
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
    try {
      // si on a pris
      if (partie.enchereController.id == id || partie.enchereController.id == idPartenaire) {
        if (pli.isEmpty) strategieAttaqueOuverture(jouables,pli)
        else if (partie.enchereController.contrat == 400) strategiePartGeneral(jouables,pli,couleurDemande.get)
        else strategieAttaque(jouables,pli,couleurDemande.get)
      }
      else {
        if (pli.isEmpty) strategieDefenseOuverture(jouables,pli)
        else strategieDefense(jouables,pli,couleurDemande.get)
      }
    } catch {
      case e:Throwable => println(s"$e pour jouables: $jouables - autres : $autres - pli: $pli");throw e
    }
  }
}
