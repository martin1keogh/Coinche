package GameLogic.Bot

import GameLogic._
import UI.Reader.{Coinche, SurCoinche}
import GameLogic.Enchere.{Pique, Undef, Couleur}
import GameLogic.Joueur
import UI.Reader.SurCoinche
import UI.Reader.Coinche
import scala.Some

trait BotTrait extends Joueur{

  val partie:Partie

  val Router = partie.Reader.router
  val mainController = partie.mainController
  val deck = new Deck

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
   * Cette function ne renvoie faux que si on est SUR que le joueur n'a plus d'atout
   * (eg, si un joueur met le 9 sous le valet, on ne sait pas s'il ne lui reste pas de l'atout)
   * @param joueur Le joueur dont on veut savoir s'il lui reste de l'atout
   * @return false si ON SAIT qu'il n'a plus d'atout, true s'il lui en reste ou si l'on ne sait pas
   */
  private def possedeAtoutsSansCompte(joueur:Joueur):Boolean = {
    if (joueur == this) return main.exists(_.couleur == couleurAtout)
    val listePlis = mainController.cartesJoueesWithPlayer.grouped(4).toList.filter(_.exists(_._1.id == joueur.id)).map(_.reverse)
    val aPisserSurTourAtout =
      listePlis.filter(_.head._2.couleur == couleurAtout) // tous les tours d'atouts
               //have to check ids (and not Joueur instances) in case joueur changed during the game
               .exists(_.exists({case (j,c) => j.id == joueur.id && c.couleur != couleurAtout}))
    if (aPisserSurTourAtout) {println("a pisser sur tour atout");return false}
    val aPisserQuandPartPasMaitre =
      listePlis.filter(l => l.head._2.couleur != l.find(_._1.id == joueur.id).map(_._2.couleur).getOrElse(l.head._2.couleur)) // Il n'a pas joue couleur demande
               .map(l=>l.take(l.indexWhere(_._1.id == joueur.id)+1)) // on retire les cartes jouées apres le joueur
               .filter(mainController.vainqueur(_,couleurAtout).Equipe != joueur.Equipe)
               .exists(_.exists({case (j,c) => j.id == joueur.id && c.couleur != couleurAtout}))
    if (aPisserQuandPartPasMaitre) {println("a pisser part maitre");return false}
    def meilleurAtoutPli(l:List[(Joueur,Card)],c:Couleur):Card = l.reduceLeft[(Joueur,Card)]({case ((j1,c1),(j2,c2)) =>
      if (c2.stronger(c,c1).getOrElse(false)) (j2,c2) else (j1,c2)
    })._2
    val aSousCoupeAvecUniqueAtoutPlusFaible =
      listePlis.filter(_.exists({case (j,c) => j.id == joueur.id && c.couleur == couleurAtout})) // tours ou il a joue de l'atout
               .map(l=>l.take(l.indexWhere(_._1.id == joueur.id)+1)) // on retire les cartes jouées apres le joueur
               .filter(mainController.vainqueur(_,couleurAtout).id != joueur.id) // il a sous coupe
               .exists(l=>listAtoutsRestants.count(_.ordreAtout < meilleurAtoutPli(l, couleurAtout).ordreAtout) == 1)
    !aSousCoupeAvecUniqueAtoutPlusFaible
  }

  /**
   * @param joueur Le joueur dont on veut savoir s'il lui reste de l'atout
   * @return false si ON SAIT qu'il n'a plus d'atout, true s'il lui en reste ou si l'on ne sait pas
   */
  def possedeAtout(joueur:Joueur):Boolean = {
    if (joueur == this) return main.exists(_.couleur == couleurAtout)
    if (nbAtoutsRestants == main.count(_.couleur == couleurAtout)) false
    else possedeAtoutsSansCompte(joueur)
  }

  /**
   * Cette methode ne devrait pas etre appelée directement, la couleur demande est directement accesible dans getCard
   * @param pli Liste des cartes sur la table, dans l'ordre inverse (derniere carte jouée en premiere position)
   *            Si l'ordre des cartes n'est pas le bon, le resultat n'est pas sur !
   * @return Option sur la couleur demandee durant ce pli
   */
  def couleurDemande(pli:List[Card]):Option[Couleur] = if (pli.isEmpty) None else Some(pli.last.couleur)

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

  def nbAtoutsJouees:Int = mainController.cartesJouees.count(_.couleur == couleurAtout)
  def nbAtoutsRestants:Int = 8 - nbAtoutsJouees
  def listAtoutsRestants = deck.sortedDeck diff mainController.cartesJouees.filter(_.couleur == couleurAtout)

  /**
   *
   * @param card
   * @return true si `card` a deja ete jouee
   */
  def dejaJoue(card:Card) = mainController.cartesJouees.contains(card)

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
  def getCarteMaitreACouleur(couleur:Couleur):Option[Card] = {
    deck.sortedDeck.diff(mainController.cartesJouees).filter(_.couleur == couleur).sortBy(card =>
      if (couleur == couleurAtout) -card.ordreAtout
      else -card.ordreClassique
    ).headOption
  }

}
