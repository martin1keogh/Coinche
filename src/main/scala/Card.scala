/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 19:34
 * To change this template use File | Settings | File Templates.
 */
class Card(n:Int) {
  val famille:Int = (n/8)

  val valeur:Int = (n%8)

  def familleToString:String = famille match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
  }

  def valeurToString:String = valeur match {
    case 0 => "Sept"
    case 1 => "Huit"
    case 2 => "Neuf"
    case 3 => "Valet"
    case 4 => "Dame"
    case 5 => "Roi"
    case 6 => "Dix"
    case 7 => "As"
  }


  def equals(c: Card): Boolean = {
    c.valeur == valeur && c.famille == famille
  }

  override def toString:String = valeurToString+" de "+familleToString
}
