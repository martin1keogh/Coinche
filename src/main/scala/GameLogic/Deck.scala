package GameLogic

import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 19:51
 * To change this template use File | Settings | File Templates.
 */
object Deck {
  val sortedDeck:List[Card] = List.tabulate(32){new Card(_)}

  def newShuffledDeck:List[Card] = {
    shuffle(sortedDeck)
  }

  def shuffle(deck:List[Card]):List[Card] = {
    Random.shuffle(deck)
  }

  /**
   *
   * @param deck Le deck a distribue
   * @return Un 4-Tuples de 4*8 cartes
   */
  def distribution(deck:List[Card]) = {
      (deck.slice(0,3)++deck.slice(12,14)++deck.slice(20,23),
       deck.slice(3,6)++deck.slice(14,16)++deck.slice(23,26),
       deck.slice(6,9)++deck.slice(16,18)++deck.slice(26,29),
       deck.slice(9,12)++deck.slice(18,20)++deck.slice(29,32)
      )
  }

  /**
   *
   * @param main La main a trier
   * @return La main trier, l'ordre des familles est quelconque,
   *         l'ordre dans une famille est sans-atout
   */
  def trierMain(main:List[Card]):List[Card] = {
    main.sortBy({card => (card.famille,-card.valeur)})
  }

  /**
   *
   * @param deck Deck a couper
   * @param nbCartesCoupees Nombre de cartes dans le paquet superieurs,
   *                        doit etre superieur strict a 3 et
   *                        et inferieur strict a 29
   * @return Le deck avec les cartes coupees mises, dans le meme ordre,
   *         dessous du paquet
   */
  @throws[IllegalArgumentException]("Si le nombre de carte a couper est invalide")
  def coupe(deck:List[Card],nbCartesCoupees:Int):Option[List[Card]] = {
    require(nbCartesCoupees > 3 && nbCartesCoupees < 29,"Erreur : coupe invalide")
    Some(deck.takeRight(nbCartesCoupees)++deck.take(32-nbCartesCoupees))
  }

}
