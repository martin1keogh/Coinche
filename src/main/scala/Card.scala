/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 19:34
 * To change this template use File | Settings | File Templates.
 */
class Card(n:Int) {
  def famille:String = (n/8) match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
  }

  def valeur:String = (n%8) match {
    case 0 => "As"
    case 1 => "Sept"
    case 2 => "Huit"
    case 3 => "Neuf"
    case 4 => "Dix"
    case 5 => "Valet"
    case 6 => "Dame"
    case 7 => "Roi"
  }


  def equals(c: Card): Boolean = {
    c.valeur == valeur && c.famille == famille
  }

  override def toString:String = valeur+" de "+famille
}
