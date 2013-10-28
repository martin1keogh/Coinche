package GameLogic

import GameLogic.Enchere._
import scala.language.implicitConversions

case class Enchere(couleur:Couleur, contrat:Int, id:Joueur.Position,nom:String, var coinche:Coinche = Normal) {

  def couleurToString: String = Enchere.couleurToInt(couleur) match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
    case 4 => "Tout Atout"
    case 5 => "Sans Atout"
  }

  def coincheToString:String = coinche match {
    case Normal => "."
    case Coinche => ", coinche!"
    case SurCoinche => ", coinche/sur-coinche!!"
  }

  override def toString = contrat+" a "+couleurToString+" par "+nom+coincheToString

  val coincheTimeout = EnchereController.coincheTimeout.fromNow // 5 seconds
  val surCoincheTimeout = EnchereController.surCoincheTimeout.fromNow // 5 seconds
  def coinchable : Boolean = coinche == Normal && coincheTimeout.hasTimeLeft()
  def surCoinchable : Boolean = coinche == Coinche && surCoincheTimeout.hasTimeLeft()
}

object Enchere {

  sealed trait Couleur
  case object Pique extends Couleur
  case object Coeur extends Couleur
  case object Carreau extends Couleur
  case object Trefle extends Couleur
  case object ToutAtout extends Couleur
  case object SansAtout extends Couleur
  case object Undef extends Couleur
  
  implicit val mainOrdering = Ordering.by{c:(Couleur,Int) => (couleurToInt(c._1),c._2)}

  implicit object CouleurOrdering extends Ordering[Couleur] {
    def compare(c1:Couleur,c2:Couleur) = {
      (couleurToInt(c1)) compare (couleurToInt(c2))
    }
  }

  sealed trait Coinche
  case object Normal extends Coinche
  case object Coinche extends Coinche
  case object SurCoinche extends Coinche

  implicit def coincheToInt(c:Coinche):Int = c match {
    case Normal => 1
    case Coinche => 2
    case SurCoinche => 4
  }

  implicit def couleurToInt(c:Couleur):Int = c match{
    case Pique => 0
    case Carreau => 1
    case Trefle => 2
    case Coeur => 3
    case ToutAtout => 4
    case SansAtout => 5
    case Undef => -1
  }

  implicit def intToCouleur(i:Int):Couleur = i match {
    case 0 => Pique
    case 1 => Carreau
    case 2 => Trefle
    case 3 => Coeur
    case 4 => ToutAtout
    case 5 => SansAtout
    case _ => Undef
  }
}
