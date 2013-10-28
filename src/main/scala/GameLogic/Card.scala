package GameLogic

import GameLogic.Enchere.{Pique, Couleur}
import scala.language.implicitConversions
import GameLogic.Card.Valeur
import Enchere._

case class Card(n:Int) {
  import Card._

  @deprecated(message = "Card.couleur should now be used.")
  val famille:Int = n / 8

  @deprecated(message = "Card.valeur should now be used.")
  val v:Int = n % 8

  val couleur:Couleur = Enchere.intToCouleur(n/8)
  val valeur:Valeur = Card.intToValeur(n%8)

  val pointsToutAtout= valeur match {
    case Sept => 0
    case Huit => 0
    case Neuf => 9
    case Valet => 14
    case Dame => 2
    case Roi => 3
    case Dix => 4
    case As => 6

  }

  val pointsSansAtout= valeur match {
    case Sept => 0
    case Huit => 0
    case Neuf => 0
    case Valet => 2
    case Dame  => 3
    case Roi => 4
    case Dix => 10
    case As => 19
  }

  val pointsClassique= valeur match {
    case Sept => 0
    case Huit => 0
    case Neuf => 0
    case Valet => 2
    case Dame  => 3
    case Roi => 4
    case Dix => 10
    case As => 11
  }


  val pointsAtout= valeur match {
    case Sept => 0
    case Huit => 0
    case Neuf => 14
    case Valet => 20
    case Dame  => 3
    case Roi => 4
    case Dix => 10
    case As => 11
  }

  val ordreAtout= valeur match {
    case Sept => 0
    case Huit => 1
    case Neuf => 6
    case Valet => 7
    case Dame  => 2
    case Roi => 3
    case Dix => 4
    case As => 5
  }

  val ordreClassique:Int = n % 8

  val valeurToString:String = Card.valeurToString(n % 8)
  val familleToString:String = Card.familleToString(n / 8)

  def equals(c: Card): Boolean = c.valeur == valeur && c.couleur == couleur

  /**
   *
   * @param couleurAtout La couleur de l'atout durant cette main
   * @param c la carte a laquelle comparer
   * @return None si les cartes ne sont pas de la meme famille et qu'aucune des deux n'est de l'atout
   *         Some(true) si la carte est plus grande que c, false sinon
   *         A sans/tout atout, none si elle ne sont pas de la meme famille, true/false sinon
   */
  def stronger(couleurAtout:Couleur,c:Card):Option[Boolean] = couleurAtout match {
    case SansAtout => if (c.couleur == couleur) Some(ordreClassique > c.ordreClassique) else None
    case ToutAtout => if (c.couleur == couleur) Some(ordreAtout > c.ordreAtout) else None
    case a =>
         // Les deux cartes sont de l'atout
         if (c.couleur == couleur && couleur == a) Some(ordreAtout > c.ordreAtout)
         // Aucune des cartes n'est de l'atout
         else if (c.couleur == couleur) Some(ordreClassique > c.ordreClassique)
         else if (c.couleur == a) Some(false)
         else if (couleur == a) Some(true)
         else None
  }

  override def toString:String = valeurToString+" de "+familleToString
}

object Card {

  def stringToFamille(s:String):Couleur = s toUpperCase() match {
    case "PIQUE" | "P" | "PI" => Pique
    case "CARREAU" | "CA" => Carreau
    case "TREFLE" | "T" | "TR" => Trefle
    case "COEUR" | "CO"  => Coeur
    case _ => Undef
  }

  def stringToValeur(s:String):Valeur = s toUpperCase() match {
    case "SEPT" | "7" => Sept
    case "HUIT" | "8" => Huit
    case "NEUF" | "9" => Neuf
    case "VALET"| "V" | "J" => Valet
    case "DAME" | "D" | "Q" => Dame
    case "ROI"  | "R" | "K" => Roi
    case "DIX"  | "10" => Dix
    case "AS"   | "A"=> As
  }

  def familleToString(famille:Int):String = famille match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
  }

  def valeurToString(valeur:Int):String = valeur match {
    case 0 => "Sept"
    case 1 => "Huit"
    case 2 => "Neuf"
    case 3 => "Valet"
    case 4 => "Dame"
    case 5 => "Roi"
    case 6 => "Dix"
    case 7 => "As"
  }

  sealed trait Valeur {def de(c:Couleur) = Card(valeurToInt(this)+c*8)}
  case object Sept extends Valeur
  case object Huit extends Valeur
  case object Neuf extends Valeur
  case object Dix extends Valeur
  case object Valet extends Valeur
  case object Dame extends Valeur
  case object Roi extends Valeur
  case object As extends Valeur

  def valeurToInt(v:Valeur):Int = v match {
    case Sept => 0
    case Huit => 1
    case Neuf => 2
    case Dix => 6
    case Valet => 3
    case Dame => 4
    case Roi => 5
    case As => 7
    case e => {println(e);-1}
  }

  private implicit def intToValeur(i:Int):Valeur = i match {
    case 0 => Sept
    case 1 => Huit
    case 2 => Neuf
    case 6 => Dix
    case 3 => Valet
    case 4 => Dame
    case 5 => Roi
    case 7 => As
  }
}
