package GameLogic.Bot

import GameLogic._
import UI.Reader.{Coinche, SurCoinche}
import GameLogic.Enchere.Couleur
import GameLogic.Joueur
import UI.Reader.SurCoinche
import UI.Reader.Coinche
import scala.Some

abstract class BotTrait(partie:Partie,id:Int,nom:String) extends Joueur(id,nom){

  val Router = partie.Reader.router

  /**
   * Renvoie l'annonce effectuer par le bot en fonction des encheres deja annoncées
   * @param listEnchere Liste des encheres deja annoncées
   * @return None si Passe, Some(Couleur,Contrat) sinon
   */
  def getCouleurEtContrat(listEnchere:List[Enchere]):Option[(Couleur,Int)]

  /**
   *
   * @param jouables Liste des cartes jouables par le bot
   * @param autres Liste des cartes non jouables
   * @param pli Liste des cartes sur la table, dans l'ordre inverse (derniere carte jouée en premiere position)
   * @return Carte jouée par le bot
   */
  def getCard(jouables:List[Card],autres:List[Card],pli:List[(Joueur,Card)])
             (implicit couleurPli:Option[Couleur] = couleurDemande(pli.map(_._2))):Card



  def surCoinche() = Router ! SurCoinche(this)
  def coinche() = Router ! Coinche(this)

  def couleurAtout:Couleur = partie.enchereController.couleur

  /**
   *
   * @return None si le partenaire n'a pas encore fait d'encheres (ou a passé), Some(meilleurEncherePart) sinon
   */
  def encherePart(listEnchere:List[Enchere]):Option[Enchere] = listEnchere.find(_.id == idPartenaire)

  /**
   *
   * @param listEnchere Liste des encheres annoncées
   * @return None si Passe, Some(Enchere) sinon
   */
  def effectuerEnchere(listEnchere:List[Enchere]):Option[Enchere] = {
    val annonce = getCouleurEtContrat(listEnchere).map({case (couleur,contrat) => new Enchere(couleur,contrat,id,nom)})
    if (annonce.exists(e => partie.enchereController.annonceLegal(this,e.contrat))) annonce else None
  }


  /**
   * Cette methode ne devrait pas etre appelée directement, la couleur demande est directement accesible dans getCard
   * @param pli Liste des cartes sur la table, dans l'ordre inverse (derniere carte jouée en premiere position)
   *            Si l'ordre des cartes n'est pas le bon, le resultat n'est pas sur !
   * @return Option sur la couleur demandee durant ce pli
   */
  def couleurDemande(pli:List[Card]):Option[Couleur] = if (pli.isEmpty) None else Some(pli.last.famille)
  /**
   *
   * @param pli Liste des cartes sur la table
   * @return Option sur le Couple (joueur,carte) maitre
   */
  def carteMaitre(pli:List[(Joueur,Card)])(implicit couleurDemande:Couleur):Option[(Joueur,Card)] = {
    val atouts = pli.filter(_._2.famille == Enchere.couleurToInt(couleurAtout))
    if (!atouts.isEmpty) atouts.sortBy(-_._2.ordreAtout).headOption
    else pli.filter(_._2.famille == Enchere.couleurToInt(couleurDemande)).sortBy(-_._2.ordreClassique).headOption
  }

  /**
   *
   * @param card
   * @return true si `card` a deja ete jouee
   */
  def dejaJoue(card:Card) = partie.mainController.cartesJouees.contains(card)

  /**
   *
   * @param pli Liste des cartes sur la table
   * @param couleurDemande
   * @return true si le partenaire est maitre, false sinon OU SI il n'y a aucune carte sur la table
   */
  def isPartMaitre(pli:List[(Joueur,Card)])(implicit couleurDemande:Couleur):Boolean = carteMaitre(pli).exists(_._1.id == idPartenaire)

  /**
   *
   * @param couleur La couleur dont on veut connaitre la carte maitre
   * @return None si toutes les cartes de cette couleur sont deja tombees, Some(meilleurCarte) sinon
   */
  def carteMaitreCouleur(couleur:Couleur):Option[Card] = {
    val deck = new Deck
    deck.sortedDeck.filter(_.famille == Enchere.couleurToInt(couleur)).sortBy(card =>
      if (couleur == couleurAtout) -card.ordreAtout
      else -card.ordreClassique
    ).headOption
  }

}
