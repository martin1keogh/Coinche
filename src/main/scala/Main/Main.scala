package Main

import GameLogic.Partie
import UI._
import UI.Console.{ReaderConsole, PrinterConsole}

object Main {

  // Default I/O, modified in main
  var Printer:Printer = PrinterConsole
  var Reader:Reader = ReaderConsole

  def parseArgs(list:List[String]): Unit = list match {
    case "--printer" :: value :: tail => Printer = Class.forName(value+"$").getField("MODULE$").get(null).asInstanceOf[Printer]
    case "--reader" :: value :: tail => Reader = Class.forName(value+"$").getField("MODULE$").get(null).asInstanceOf[Reader]
    case _ => ()
  }

  def main(args: Array[String]) {

    parseArgs(List[String]("--printer","UI.Console.PrinterConsole"))

    Partie.start()
  }
}
