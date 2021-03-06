package GameLogic

import scala.util.Random
import Enchere._

class Deck {
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
   * @return Une liste de listes de 8 cartes
   */
  def distribution(deck:List[Card]):List[List[Card]] = {
    val m1 = deck.slice(0, 3) ++ deck.slice(12, 14) ++ deck.slice(20, 23)
    val m2 = deck.slice(3, 6) ++ deck.slice(14, 16) ++ deck.slice(23, 26)
    val m3 = deck.slice(6, 9) ++ deck.slice(16, 18) ++ deck.slice(26, 29)
    val m4 = deck.slice(9, 12) ++ deck.slice(18, 20) ++ deck.slice(29, 32)
    m1::m2::m3::m4::List[List[Card]]()
  }

  /**
   *
   * @param main La main a trier
   * @return La main trier, l'ordre des familles est quelconque,
   *         l'ordre dans une famille est sans-atout
   */
  def trierMain(main:List[Card]):List[Card] = {
    main.sortBy({card => (card.couleur,-card.ordreClassique)})
  }

  def trierMain(main:List[Card],couleurAtout:Couleur):List[Card] = couleurAtout match{
    case SansAtout => trierMain(main)
    case ToutAtout => main.sortBy({card => (card.couleur,-card.pointsToutAtout)})
    case c:Couleur => main.sortBy({card => if (card.couleur == c) (card.couleur,-card.pointsAtout)
                                   else (card.couleur,-card.pointsClassique) })
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
