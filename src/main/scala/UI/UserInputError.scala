package mk.coinche.ui

sealed trait UserInputError
case class ParseError(input: String) extends UserInputError
case class InvalidPlay(input: String) extends UserInputError
