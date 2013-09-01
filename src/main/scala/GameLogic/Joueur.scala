package GameLogic

import scala.language.implicitConversions

object Joueur {
  sealed trait Position
  case object Sud extends Position
  case object Ouest extends Position
  case object Nord extends Position
  case object Est extends Position
  case object Undef extends Position

  sealed trait Equipe
  case object NordSud extends Equipe
  case object EstOuest extends Equipe
  case object UndefEquipe extends Equipe
}

import Joueur._

case class Joueur(id:Position, var nom:String) {

  lazy val idPartenaire:Position = id match {
    case Sud => Nord
    case Ouest => Est
    case Nord => Sud
    case Est => Ouest
    case Undef => {println("Called idPartenaire for Joueur(Undef) !"); Undef}
  }

  val equipe = id match {
    case Sud | Nord => NordSud
    case Est | Ouest => EstOuest
    case Undef => UndefEquipe
  }

  lazy val nextPosition:Position = id match {
    case Sud => Ouest
    case Ouest => Nord
    case Nord => Est
    case Est => Sud
    case Undef => {println("called nextPlayer(Undef)!"); Undef}
  }

  var main = List[Card]()

  def rename(n:String):Unit = nom = n

  override def toString = nom
}
