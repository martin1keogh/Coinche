package Main

import GameLogic.Partie
import UI.Console.{ReaderConsole, PrinterConsole}

object Main {

  def main(args: Array[String]) {

    val printer = new PrinterConsole
    val reader = new ReaderConsole(printer)
    val Partie = new Partie(printer,reader)

    Partie.start()
  }
}
