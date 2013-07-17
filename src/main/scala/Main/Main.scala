package Main

import GameLogic.Partie
import UI._
import UI.Console.{ReaderConsole, PrinterConsole}

object Main {

  def main(args: Array[String]) {

    val Partie = new Partie

    Partie.start()
  }
}
