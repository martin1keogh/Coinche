package GameLogic

/**
 * Created by martin on 18/07/13.
 */
class Enchere(val couleur:Int,val contrat:Int,val id:Int,nom:String, var coinche:Int = 1) {

  def couleurToString:String = couleur match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
    case 4 => "Tout Atout"
    case 5 => "Sans Atout"
  }

  def coincheToString:String = coinche match {
    case 1 => "."
    case 2 => ", coinche!"
    case 4 => ", coinche/sur-coinche!!"
  }

  override def toString = contrat+" a "+couleurToString+" par "+nom+coincheToString

}
