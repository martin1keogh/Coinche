package mk.coinche.models

import enumeratum._

sealed trait Rank extends EnumEntry

object Rank extends Enum[Rank] {
  val values = findValues

  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine  extends Rank
  case object Ten   extends Rank
  case object Jack  extends Rank
  case object Queen extends Rank
  case object King  extends Rank
  case object As    extends Rank
}

