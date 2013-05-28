package UI

import GameLogic.{Partie,Card}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 22:13
 * To change this template use File | Settings | File Templates.
 */
object Reader {

  @throws[Exception]("Fin de la partie.")
  def getCouleur:Int = {
    var couleur = -1
    do {
      println("Quelle couleur (entree pour passer, h pour l'aide, quit pour quitter) ?")
      println("1/Pique;2/Carreau;3/Trefle;4/Coeur;5/Tout atout;6/Sans Atout")
      couleur = try {
        val c = readLine()
        Printer.printSmth(c)
        println()
        if (c == "") 0 // tres sale,permet de gerer le <entree> pour passer son tour
        else if (c == "quit") sys.exit()
        else c.toInt
      }
      catch {
        case e:NumberFormatException => -1
      }
    } while (couleur < 0 || couleur > 6)
    couleur
  }

  def getContrat:Int = {
    println("Quelle annonce ?")
    val c = readLine()
    Printer.printSmth(c)
    println()
    try {c.toInt}
    catch {case e:NumberFormatException => -1}
  }

  def getCard(jouables:List[Card],autres:List[Card]):Card = {
    Printer.printCartes(jouables,autres)
    val c = readLine()
    if (c == "quit") sys.exit()
    if (c == "e") Printer.printEnchere()
    try {jouables(c.toInt)}
    catch {
      case e:NumberFormatException => getCard(jouables,autres)
      case e:IndexOutOfBoundsException => getCard(jouables,autres)
    }
  }


}
