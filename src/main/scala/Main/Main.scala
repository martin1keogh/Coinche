package Main

import GameLogic.Partie
import UI.Console.{PrinterConsole, ReaderConsole}
import scala.concurrent.Future
import GameLogic.Bot.DumBot

object Main {

  def main(args: Array[String]) {

    import scala.concurrent.ExecutionContext.Implicits.global

    val partie = new Partie(new PrinterConsole,new ReaderConsole)
    partie.j1 = new DumBot(partie,0,"Bot1")
    partie.j2 = new DumBot(partie,1,"Bot2")
    partie.j3 = new DumBot(partie,2,"Bot3")
    partie.j4 = new DumBot(partie,3,"Bot4")

    Future{partie.start()}

    while (true) {
      val read = readLine()
      partie.Reader.sendMessage(partie.currentPlayer,read.asInstanceOf[partie.Reader.Input])
    }
  }
}
