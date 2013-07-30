package GameLogic

import GameLogic.Enchere.Couleur
import scala.language.implicitConversions

class Enchere(val couleur:Couleur,val contrat:Int,val id:Int,nom:String, var coinche:Int = 1) {

  def couleurToString: String = Enchere.couleurToInt(couleur) match {
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

object Enchere {
  sealed abstract class Couleur
  case class Pique() extends Couleur
  case class Coeur() extends Couleur
  case class Carreau() extends Couleur
  case class Trefle() extends Couleur
  case class ToutAtout() extends Couleur
  case class SansAtout() extends Couleur
  case class Undef() extends Couleur

  implicit def couleurToInt(c:Couleur):Int = c match{
    case Pique() => 0
    case Carreau() => 1
    case Trefle() => 2
    case Coeur() => 3
    case ToutAtout() => 4
    case SansAtout() => 5
    case Undef() => -1
  }

  implicit def intToCouleur(i:Int):Couleur = i match {
    case 0 => Pique()
    case 1 => Carreau()
    case 2 => Trefle()
    case 3 => Coeur()
    case 4 => ToutAtout()
    case 5 => SansAtout()
    case _ => Undef()
  }
}
