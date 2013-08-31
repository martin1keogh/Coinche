package Main

import GameLogic.Partie
import UI.Console.{PrinterConsole, ReaderConsole}
import scala.concurrent.Future
import GameLogic.Bot.DumBot

object Main {

  def main(args: Array[String]) {

    import scala.concurrent.ExecutionContext.Implicits.global

    val partie = new Partie(new PrinterConsole,new ReaderConsole)
    partie.playerToBot(partie.j2,DumBot.createFromPlayer(partie,partie.j2))
    partie.playerToBot(partie.j3,DumBot.createFromPlayer(partie,partie.j3))
    partie.playerToBot(partie.j4,DumBot.createFromPlayer(partie,partie.j4))

    Future{partie.start()}

    while (true) {
      val read = readLine()
      partie.Reader.sendMessage(partie.currentPlayer,read.asInstanceOf[partie.Reader.Input])
      if (read == "toBot") partie.playerToBot(partie.j1,DumBot.createFromPlayer(partie,partie.j1,"Bot"))
    }
  }
}
