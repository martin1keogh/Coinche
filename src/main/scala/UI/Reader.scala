package UI

import GameLogic.{Joueur, Card}

trait Reader {

  /**
   * Renvoie la couleur annoncé :
   *    1 => pique
   *    2 => carreau
   *    3 => trefle
   *    4 => coeur
   *    5 => tout atout
   *    6 => sans atout
   *
   *    0 => passe
   *
   *    7 => coinche
   *    8 => sur-coinche
   *
   * Ne doit renvoyer qu'un entier convenable !
   * Doit verifier que la personne qui coinche peut le faire (i.e est dans l'equipe opposée)
   *
   * @return la couleur annoncé
   */
  def getCouleur:Int

  /**
   * Renvoie la valeur du contrat
   * @return le contrat
   */
  def getContrat:Int

  /**
   *
   * @return le joueur qui a surcoinche, ou None si personne n'a coinche apres 5 secondes
   */
  def getSurCoinche:Option[Joueur]

  /**
   * Renvoie la carte jouée (doit etre dans jouable)
   * @param jouables les cartes autorisées
   * @param autres les cartes non jouables
   * @return la carte jouée
   */
  def getCard(jouables:List[Card],autres:List[Card]):Card

}
