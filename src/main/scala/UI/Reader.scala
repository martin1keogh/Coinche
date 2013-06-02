package UI

import GameLogic.Card

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 02/06/13
 * Time: 19:27
 * To change this template use File | Settings | File Templates.
 */
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
   * Ne doit renvoyer qu'un entier convenable !
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
   * Renvoie la carte jouée (doit etre dans jouable)
   * @param jouables les cartes autorisées
   * @param autres les cartes non jouables
   * @return la carte jouée
   */
  def getCard(jouables:List[Card],autres:List[Card]):Card

}
