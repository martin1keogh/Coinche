package Main

import GameLogic.Partie
import UI._
import UI.Console.{ReaderConsole, PrinterConsole}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 20:23
 * To change this template use File | Settings | File Templates.
 */
object Main {

  // Default I/O, modified in main
  var Printer:Printer = PrinterConsole
  var Reader:Reader = ReaderConsole


  def main(args: Array[String]) {

    Partie.start()
  }
}
