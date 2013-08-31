package GameLogic.Bot

import GameLogic._
import GameLogic.Enchere._
import UI.Reader.SurCoinche
import GameLogic.Joueur
import UI.Reader.Coinche
import GameLogic.Card.Valeur

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
   * @param listEnchere La liste des encheres parmi lesquelles chercher
   * @return None si le partenaire n'a pas encore fait d'encheres (ou a passé), Some(meilleurEncherePart) sinon
   */
  def encherePart(listEnchere:List[Enchere]):Option[Enchere] = listEnchere.find(_.id == idPartenaire)

  def encherePart:Option[Enchere] = partie.enchereController.listEnchere.find(_.id == idPartenaire)

  /**
   *
   * @param listEnchere Liste des encheres annoncées
   * @return None si Passe, Some(Enchere) sinon
   */
  def effectuerEnchere(listEnchere:List[Enchere]):Option[Enchere] = {
    val annonce = getCouleurEtContrat(listEnchere).map({case (couleur,contrat) => new Enchere(couleur,contrat,id,nom)})
    if (annonce.exists(e => partie.enchereController.annonceLegal(this,e.contrat))) annonce else None
  }

  def meilleurAtoutPli(pli:List[(Joueur,Card)]):Option[Card] =
    pli.reduceLeftOption[(Joueur,Card)]({case ((j1,c1),(j2,c2)) =>
      if (c2.stronger(couleurAtout,c1).getOrElse(false)) (j2,c2) else (j1,c1)
    }).map(_._2).filter(_.couleur == couleurAtout)

  /**
   * Cette function ne renvoie faux que si on est SUR que le joueur n'a plus d'atout
   * (eg, si un joueur met le 9 sous le valet, on ne sait pas s'il ne lui reste pas de l'atout)
   *
   * Cette fonction ne devrait certainement pas etre appelé : elle ne tient pas compte des atouts dans la main du bot.
   * Utiliser possedeAtouts(joueur) plutot.
   *
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
    if (aPisserSurTourAtout) return false

    val aPisserQuandPartPasMaitre =
      listePlis.filter(l => l.head._2.couleur != l.find(_._1.id == joueur.id).map(_._2.couleur).getOrElse(l.head._2.couleur)) // Il n'a pas joue couleur demande
               .map(l=>l.take(l.indexWhere(_._1.id == joueur.id)+1)) // on retire les cartes jouées apres le joueur
               .filter(mainController.vainqueur(_,couleurAtout).Equipe != joueur.Equipe)
               .exists(_.exists({case (j,c) => j.id == joueur.id && c.couleur != couleurAtout}))
    if (aPisserQuandPartPasMaitre) return false

    val aSousCoupeAvecUniqueAtoutPlusFaible =
      listePlis.filter(_.exists({case (j,c) => j.id == joueur.id && c.couleur == couleurAtout})) // tours ou il a joue de l'atout
               .map(l=>l.take(l.indexWhere(_._1.id == joueur.id)+1)) // on retire les cartes jouées apres le joueur
               .filter(mainController.vainqueur(_,couleurAtout).id != joueur.id) // il a sous coupe
               .exists(l=> listAtoutsRestants.find(_.ordreAtout < meilleurAtoutPli(l).get.ordreAtout).isEmpty)
    !aSousCoupeAvecUniqueAtoutPlusFaible
  }

  /**
   * @param joueur Le joueur dont on veut savoir s'il lui reste de l'atout
   * @return false si ON SAIT qu'il n'a plus d'atout, true s'il lui en reste ou si l'on ne sait pas
   */
  def possedeAtout(joueur:Joueur):Boolean = {
    if (couleurAtout == ToutAtout || couleurAtout == SansAtout) false // personne n'a d'atout a ToutAtout/SansAtout
    else if (joueur == this) main.exists(_.couleur == couleurAtout)
    else if (nbAtoutsRestants == main.count(_.couleur == couleurAtout)) false // si on a tous les atouts restants, `joueur` n'en a pas
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
   * @param couleurDemande couleur de la carte ayant ouvert le pli
   * @return Option sur le Couple (joueur,carte) maitre
   */
  def carteEtJoueurMaitre(pli:List[(Joueur,Card)],couleurDemande:Option[Couleur]):Option[(Joueur,Card)] = {
    val atouts = pli.filter(_._2.famille == Enchere.couleurToInt(couleurAtout))
    if (!atouts.isEmpty) atouts.sortBy(-_._2.ordreAtout).headOption
    else pli.filter(_._2.famille == Enchere.couleurToInt(couleurDemande.get)).sortBy(-_._2.ordreClassique).headOption
  }

  def carteMaitre(pli:List[Card],couleurDemande:Option[Couleur]):Option[Card] = {
    val atouts = pli.filter(_.famille == Enchere.couleurToInt(couleurAtout))
    if (!atouts.isEmpty) atouts.sortBy(-_.ordreAtout).headOption
    else pli.filter(_.famille == Enchere.couleurToInt(couleurDemande.get)).sortBy(-_.ordreClassique).headOption
  }

  def carteMaitre:Option[Card] = carteMaitre(mainController.pli.map(_._2),couleurDemande(mainController.pli.map(_._2)))

  def nbAtoutsJouees:Int = mainController.cartesJouees.count(_.couleur == couleurAtout)
  def nbAtoutsRestants:Int = 8 - nbAtoutsJouees
  def listAtoutsRestants = (deck.sortedDeck filter (_.couleur == couleurAtout)) diff mainController.cartesJouees.filter(_.couleur == couleurAtout)

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
   * @return true si le partenaire est maitre, false sinon OU SI le part n'a pas joué
   */
  def isPartMaitre(pli:List[(Joueur,Card)],couleurDemande:Option[Couleur]):Boolean =
    carteEtJoueurMaitre(pli,couleurDemande).exists(_._1.id == idPartenaire)

  /**
   *
   * @param pli Liste des cartes sur la table
   * @param couleurDemande
   * @return true si le partenaire est maitre ET le restera sauf en cas de coupe, false si non ou si le part n'a pas joue
   */
  def partGagnePliSaufCoupe(pli:List[(Joueur,Card)],couleurDemande:Couleur):Boolean ={
    val cartePart = pli.find(_._1.id == idPartenaire)
    if (cartePart.isEmpty) false
    else getValeurMaitreACouleur(couleurDemande).get == cartePart.get._2.valeur
  }


  /**
   *
   * @param couleur La couleur dont on veut connaitre la carte maitre
   * @return None si toutes les cartes de cette couleur sont deja tombees, Some(meilleurValeur) sinon
   */
  def getValeurMaitreACouleur(couleur:Couleur):Option[Valeur] = {
    deck.sortedDeck.diff(mainController.cartesJouees).filter(_.couleur == couleur).sortBy(card =>
      if (couleurAtout == ToutAtout || couleur == couleurAtout) -card.ordreAtout
      else -card.ordreClassique
    ).headOption.map(_.valeur)
  }

  /**
   *
   * @param couleur La couleur dont on veut connaitre la carte maitre
   * @param pli Liste de cartes a rajouter dans les cartes jouées
   * @return None si toutes les cartes de la couleur sont deja tombees, Some(meilleurValeur) sinon
   */
  def getValeurMaitreACouleurApresPli(couleur:Couleur,pli:List[Card]):Option[Valeur] = {
    deck.sortedDeck.diff(mainController.cartesJouees).diff(pli).filter(_.couleur == couleur).sortBy(card =>
      if (couleurAtout == ToutAtout || couleur == couleurAtout) -card.ordreAtout
      else -card.ordreClassique
    ).headOption.map(_.valeur)
  }

}
