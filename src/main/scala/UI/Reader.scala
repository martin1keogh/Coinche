package UI

import GameLogic.{Joueur, Card}
import GameLogic.Enchere.Couleur

trait Reader {

  def getMessage:Reader.Message

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

object Reader{
  abstract class Message
  case class Bid(couleur:Couleur,valeur:Int,j:Joueur) extends Message
  case class Coinche(j:Joueur) extends Message
  case class SurCoinche(j:Joueur) extends Message
  case class Passe() extends Message

}
