package UI

import GameLogic.Card
import GameLogic.Enchere.Couleur
import akka.actor.{Props, ActorSystem}
import UI.Reader._
import GameLogic.Joueur
import UI.Reader.PlayCard
import scala.Some

abstract class Reader{

  type Input

  val system = ActorSystem("system")
  val router = system.actorOf(Props[Router])


  def inputToCard(input:Input):List[Card]

  /**
   * Tries to transform input (type A) to card
   * @param joueur player who inputted text
   * @param input
   * @return None if input was not recognized, Some(PlayCard(joueur,inputToCard(input)) otherwise
   */
  implicit def inputToPlayingMessageOption(joueur:Joueur,input:Input):Option[PlayingMessage] = {
    val cardList = inputToCard(input)
    if (!cardList.isEmpty) Some(PlayCard(joueur,cardList))
    else None
  }

  /**
   * Tries to transform input to bidding message
   * @param joueur
   * @param input
   * @return None if input was not recognized, Some(message:BiddingMessage) otherwise
   */
  implicit def inputToBiddingMessageOption(joueur:Joueur,input:Input):Option[BiddingMessage]

  def sendMessage(joueur:Joueur,input:Input):Unit = {
    val message = inputToBiddingMessageOption(joueur,input) orElse inputToPlayingMessageOption(joueur,input)
    if (message.isDefined) router ! message.get
  }

  def sendMessage(message:Message) = router ! message

  def sendMessage(messageOption:Option[Message]) = if (messageOption.isDefined) router ! messageOption.get

  def stopGame = router ! StopGame

//  def getMessage:(Joueur,Reader.Message)

  /**
   *
   * @return la liste de toutes les personnes qui ont surcoinche durant les 5 secondes
   */
//  def getSurCoinche:List[Joueur]

  /**
   * Renvoie la carte jouée (doit etre dans jouable)
   * @param jouables les cartes autorisées
   * @param autres les cartes non jouables
   * @return la carte jouée
   */
//  def getCard(jouables:List[Card],autres:List[Card]):Card
}

object Reader{
  abstract class Message
  abstract class BiddingMessage extends Message
  abstract class PlayingMessage extends Message

  case object StopGame extends Message
  case object StopWaiting extends Message

  case class Bid(joueur:Joueur,couleur:Couleur,valeur:Int) extends BiddingMessage
  case class Coinche(joueur:Joueur) extends BiddingMessage
  case class SurCoinche(joueur:Joueur) extends BiddingMessage
  case class Passe(joueur:Joueur) extends BiddingMessage

  case class PlayCard(joueur:Joueur,card:List[Card]) extends PlayingMessage

}
