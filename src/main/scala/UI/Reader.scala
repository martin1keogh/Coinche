package UI

import GameLogic.{Joueur, Card}
import GameLogic.Enchere.Couleur

trait Reader {

  def getMessage:(Joueur,Reader.Message)

  /**
   *
   * @return la liste de toutes les personnes qui ont surcoinche durant les 5 secondes
   */
  def getSurCoinche:List[Joueur]

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
  case class Bid(couleur:Couleur,valeur:Int) extends Message
  case class Coinche() extends Message
  case class SurCoinche() extends Message
  case class Passe() extends Message

}
