package UI.Console

import GameLogic.{EnchereController, Card}
import UI.Reader

class ReaderConsole(PrinterConsole:PrinterConsole) extends Reader{

  @throws[Exception]("Fin de la partie.")
  def getCouleur:Int = {
    var couleur = -1
    do {
      println("Quelle couleur (entree pour passer, h pour l'aide, quit pour quitter) ?")
      println("1/Pique;2/Carreau;3/Trefle;4/Coeur;5/Tout atout;6/Sans Atout")
      couleur = try {
        val c = readLine()
        println()
        if (c == "") 0 // tres sale,permet de gerer le <entree> pour passer son tour
        else if (c == "quit") sys.exit()
        else if (c == "coinche") 7
        else if (c == "sur") 8
        else c.toInt
      }
      catch {
        case e:NumberFormatException => -1
      }
    } while (couleur < 0 || couleur > 8)
    couleur
  }

  def getContrat:Int = {
    println("Quelle annonce ?")
    val c = readLine()
    println()
    try {c.toInt}
    catch {case e:NumberFormatException => -1}
  }

  def getCard(jouables:List[Card],autres:List[Card]):Card = {
    val c = readLine()
    if (c == "quit") sys.exit()
    try {jouables(c.toInt)}
    catch {
      case e:NumberFormatException => getCard(jouables,autres)
      case e:IndexOutOfBoundsException => getCard(jouables,autres)
    }
  }


}
