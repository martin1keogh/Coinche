package GameLogic

import GameLogic.Enchere.Couleur
import GameLogic.Card.Valeur

case class Card(n:Int) {
  val famille:Int = n / 8

  val v:Int = n % 8

  val couleur = Enchere.intToCouleur(famille)
  val valeur:Valeur = Card.valeurToInt(n%8)

  val pointsToutAtout:Int = v match {
    case 0 => 0  // Sept
    case 1 => 0  // Huit
    case 2 => 9  // Neuf
    case 3 => 14 // Valet
    case 4 => 2  // Dame
    case 5 => 3  // Roi
    case 6 => 4  // Dix
    case 7 => 6  // As

  }

  val pointsSansAtout:Int = v match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 0   // Neuf
    case 3 => 2   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 19  // As
  }

  val pointsClassique:Int = v match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 0   // Neuf
    case 3 => 2   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 11  // As
  }


  val pointsAtout:Int = v match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 14   // Neuf
    case 3 => 20   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 11  // As
  }

  val ordreAtout:Int = v match {
    case 0 => 0   // Sept
    case 1 => 1   // Huit
    case 2 => 6   // Neuf
    case 3 => 7   // Valet
    case 4 => 2   // Dame
    case 5 => 3   // Roi
    case 6 => 4  // Dix
    case 7 => 5  // As
  }

  val ordreClassique:Int = v

  val valeurToString:String = Card.valeurToString(v)
  val familleToString:String = Card.familleToString(famille)

  def equals(c: Card): Boolean = c.valeur == valeur && c.famille == famille

  /**
   *
   * @param couleurAtout La couleur de l'atout durant cette main
   * @param c la carte a laquelle comparer
   * @return None si les cartes ne sont pas de la meme famille et qu'aucune des deux n'est de l'atout
   *         Some(true) si la carte est plus grande que c, false sinon
   *         A sans/tout atout, none si elle ne sont pas de la meme famille, true/false sinon
   */
  def stronger(couleurAtout:Int,c:Card):Option[Boolean] = couleurAtout match {
    // Sans atout
    case 5 => if (c.famille == famille) Some(ordreClassique > c.ordreClassique) else None
    // Tout Atout
    case 4 => if (c.famille == famille) Some(ordreAtout > c.ordreAtout) else None
    case a =>
         // Les deux cartes sont de l'atout
         if (c.famille == famille && famille == a) Some(ordreAtout > c.ordreAtout)
         // Aucune des cartes n'est de l'atout
         else if (c.famille == famille) Some(ordreClassique > c.ordreClassique)
         else if (c.famille == a) Some(false)
         else if (famille == a) Some(true)
         else None
  }

  override def toString:String = valeurToString+" de "+familleToString
}

object Card {

  def stringToFamille(s:String):Int = s toUpperCase() match {
    case "PIQUE" | "P" | "PI" => 0
    case "CARREAU" | "CA" => 1
    case "TREFLE" | "T" | "TR" => 2
    case "COEUR" | "CO"  => 3
    case _ => -1
  }

  def stringToValeur(s:String):Int = s toUpperCase() match {
    case "SEPT" | "7" => 0
    case "HUIT" | "8" => 1
    case "NEUF" | "9" => 2
    case "VALET"| "V" | "J" => 3
    case "DAME" | "D" | "Q" => 4
    case "ROI"  | "R" | "K" => 5
    case "DIX"  | "10" => 6
    case "AS"   | "A"=> 7
    case _ => -1
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

  sealed trait Valeur
  case object Sept extends Valeur
  case object Huit extends Valeur
  case object Neuf extends Valeur
  case object Dix extends Valeur
  case object Valet extends Valeur
  case object Dame extends Valeur
  case object Roi extends Valeur
  case object As extends Valeur

  implicit def valeurToInt(v:Valeur):Int = v match {
    case Sept => 0
    case Huit => 1
    case Neuf => 2
    case Dix =>6
    case Valet =>3
    case Dame => 4
    case Roi => 5
    case As => 7
  }

  implicit def intToValeur(i:Int):Valeur = i match {
    case 0 => Sept
    case 1 => Huit
    case 2 => Neuf
    case 6 => Dix
    case 3 => Valet
    case 4 => Dame
    case 5 => Roi
    case 7 => As
  }

  implicit def valeurEtCouleurToCard(v:Valeur,c:Couleur):Card = Card(v+c*8)
}
