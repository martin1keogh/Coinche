package Main

import GameLogic.Partie
import UI.Console.{PrinterConsole, ReaderConsole}

object Main {

  def main(args: Array[String]) {

    val partie = new Partie(new PrinterConsole,new ReaderConsole)
    partie.start()
  }
}
