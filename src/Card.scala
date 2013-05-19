/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 19:34
 * To change this template use File | Settings | File Templates.
 */
class Card(n:Int) {
  def valeur:String = (n%4) match {
    case 0 => "Pique"
    case 1 => "Trefle"
    case 2 => "Carreau"
    case 3 => "Coeur"
  }

  def famille:String = (n%13) match {
    case 0 => "As"
    case 1 => "Deux"
    case 2 => "Trois"
    case 3 => "Quatre"
    case 4 => "Cinq"
    case 5 => "Six"
    case 6 => "Sept"
    case 7 => "Huit"
    case 8 => "Neuf"
    case 9 => "Dix"
    case 10 => "Valet"
    case 11 => "Dame"
    case 12 => "Roi"
  }
}
