package GameLogic

class Card(n:Int) {
  val famille:Int = (n/8)

  val valeur:Int = (n%8)

  val pointsToutAtout:Int = valeur match {
    case 0 => 0  // Sept
    case 1 => 0  // Huit
    case 2 => 9  // Neuf
    case 3 => 14 // Valet
    case 4 => 2  // Dame
    case 5 => 3  // Roi
    case 6 => 4  // Dix
    case 7 => 6  // As

  }

  val pointsSansAtout:Int = valeur match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 0   // Neuf
    case 3 => 2   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 19  // As
  }

  val pointsClassique:Int = valeur match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 0   // Neuf
    case 3 => 2   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 11  // As
  }


  val pointsAtout:Int = valeur match {
    case 0 => 0   // Sept
    case 1 => 0   // Huit
    case 2 => 14   // Neuf
    case 3 => 20   // Valet
    case 4 => 3   // Dame
    case 5 => 4   // Roi
    case 6 => 10  // Dix
    case 7 => 11  // As
  }

  val ordreAtout:Int = valeur match {
    case 0 => 0   // Sept
    case 1 => 1   // Huit
    case 2 => 6   // Neuf
    case 3 => 7   // Valet
    case 4 => 2   // Dame
    case 5 => 3   // Roi
    case 6 => 4  // Dix
    case 7 => 5  // As
  }

  val ordreClassique:Int = valeur

  val valeurToString:String = Card.valeurToString(valeur)
  val familleToString:String = Card.familleToString(famille)

  def equals(c: Card): Boolean = {
    c.valeur == valeur && c.famille == famille
  }

  /**
   *
   * @param couleurAtout
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
    case "AS" => 7
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

}
