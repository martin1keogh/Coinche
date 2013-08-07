package UI.Console

import UI.Reader
import GameLogic.Card
import UI.Reader._
import GameLogic.Joueur
import UI.Reader.SurCoinche
import UI.Reader.Passe
import UI.Reader.Coinche
import scala.Some

class ReaderConsole() extends Reader{

  type Input = String

  def inputToCard(input: Input): List[Card] =
    try {
      val cardInfo = input.toString.split(' ')
      // if player only supplied a card value, we check if a (playable) color corresponds
      if (cardInfo.length == 2) {
        val valeur = Card.stringToValeur(cardInfo(1))
        if (valeur == -1) List.empty[Card]
        else List.tabulate(4)(i => new Card(valeur + i * 8))
      } else {
        val famille = Card.stringToFamille(cardInfo(2))
        val valeur = Card.stringToValeur(cardInfo(1))
        if (famille == -1 || valeur == -1) List.empty[Card]
        else List(new Card(famille*8 + valeur))
      }
    } catch {
      case e:Throwable => List.empty[Card]
    }

  /**
   * Tries to transform input to bidding message
   * @param joueur Player who sent the message
   * @param input Message to try and transform
   * @return None if input was not recognized, Some(message:BiddingMessage) otherwise
   */
  implicit def inputToBiddingMessageOption(joueur: Joueur, input: Input): Option[BiddingMessage] = input.toLowerCase match {
    case "passe" => Some(Passe(joueur))
    case "coinche" => Some(Coinche(joueur))
    case "sur" => Some(SurCoinche(joueur))
    case s if s.startsWith("bid") => {
      try {
        val couleur = s.toString.split(' ')(2) match {
          case "pique" | "p" => 0
          case "carreau" | "ca" => 1
          case "trefle" | "t"=> 2
          case "coeur" | "co"=> 3
          case "ta" => 4
          case "sa" => 5
        }
        val contrat = s.toString.split(' ')(1).toInt
        Some(Bid(joueur,couleur,contrat))
      } catch {case e:Throwable =>  None}
    }
    case _ => None
  }
}
