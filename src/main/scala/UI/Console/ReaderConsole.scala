package UI.Console

import UI.Reader
import GameLogic.{Card, Joueur}
import UI.Reader.BiddingMessage

class ReaderConsole() extends Reader{

  type Input = String

  def inputToCard(input: Input): List[Card] = {List[Card]()}

  /**
   * Tries to transform input to bidding message
   * @param joueur
   * @param input
   * @return None if input was not recognized, Some(message:BiddingMessage) otherwise
   */
  implicit def inputToBiddingMessageOption(joueur: Joueur, input: Input): Option[BiddingMessage] = None
}
