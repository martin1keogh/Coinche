package GameLogic

import scala.language.implicitConversions

object Joueur {
  sealed trait Position
  case object Sud extends Position
  case object Ouest extends Position
  case object Nord extends Position
  case object Est extends Position
  case object Undef extends Position

  // legacy
  implicit def idToInt(id:Position):Int = id match {
    case Sud => 0
    case Ouest => 1
    case Nord => 2
    case Est => 3
    case Undef => {println("Called idToInt(Undef) !"); -1}
  }
}

import Joueur._

case class Joueur(id:Position, var nom:String) {

  def idPartenaire:Position = id match {
    case Sud => Nord
    case Ouest => Est
    case Nord => Sud
    case Est => Ouest
    case Undef => {println("Called idPartenaire for Joueur(Undef) !"); Undef}
  }

  def Equipe = {
    if (id%2 == 0) 'NS
    else 'EO
  }

  var main = List[Card]()

  def rename(n:String):Unit = nom = n

  override def toString = nom
}
