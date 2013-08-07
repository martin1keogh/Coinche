package Main

import GameLogic.Partie
import UI.Console.{PrinterConsole, ReaderConsole}
import scala.concurrent.Future

object Main {

  def main(args: Array[String]) {

    import scala.concurrent.ExecutionContext.Implicits.global

    val partie = new Partie(new PrinterConsole,new ReaderConsole)
    Future{partie.start()}
    while (true) {
      val read = readLine()
      partie.Reader.sendMessage(partie.currentPlayer,read.asInstanceOf[partie.Reader.Input])
    }
  }
}
