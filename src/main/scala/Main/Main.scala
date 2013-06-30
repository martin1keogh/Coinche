package Main

import GameLogic.Partie
import UI._
import UI.Console.{ReaderConsole, PrinterConsole}

object Main {

  // Default I/O, modified in main
  var Printer:Printer = PrinterConsole
  var Reader:Reader = ReaderConsole

  def parseArgs(list:List[String]): Unit = list match {
    case "--printer" :: value :: tail => {Printer = Class.forName(value+"$").getField("MODULE$").get(null).asInstanceOf[Printer];
                                          parseArgs(tail)}
    case "--reader" :: value :: tail => {Reader = Class.forName(value+"$").getField("MODULE$").get(null).asInstanceOf[Reader];
                                         parseArgs(tail)}
    case _ => ()
  }

  def main(args: Array[String]) {

//    FIXME : Appears to block
//    parseArgs(args.toList)

    Partie.start()
  }
}
