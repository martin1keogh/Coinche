package mk.coinche.models

import enumeratum._
import shapeless._, nat._

sealed trait Team
case object NorthSouth extends Team
case object WestEast extends Team

sealed trait Position extends EnumEntry {
  def team: Team
  def sameTeamAs(other: Position) = team == other.team
}

object Position extends Enum[Position] {
  val values = findValues

  case object North extends Position { val team = NorthSouth }
  case object West  extends Position { val team = WestEast }
  case object South extends Position { val team = NorthSouth }
  case object East  extends Position { val team = WestEast }

  val list = Sized[List](North, West, South, East)

  def startingFrom(p: Position): Sized[List[Position], _4] = {
    val doubled: Sized[List[Position], _8] = list ++ list
    p match {
      case North => doubled.drop(0).take(4)
      case West  => doubled.drop(1).take(4)
      case South => doubled.drop(2).take(4)
      case East  => doubled.drop(3).take(4)
    }
  }

  def after(p: Position): Position = {
    startingFrom(p).tail.head
  }
}
