package UI.Console

import GameLogic.Card
import UI.Reader
import UI.Reader._
import GameLogic.Joueur

class ReaderConsole(PrinterConsole:PrinterConsole) extends Reader{

  import GameLogic.Enchere._

  def getMessage:Message = {
    def getCouleur:Int = {
      var couleur = -1
      do {
        println("Quelle couleur (entree pour passer, h pour l'aide, quit pour quitter) ?")
        println("1/Pique;2/Carreau;3/Trefle;4/Coeur;5/Tout atout;6/Sans Atout;7/coincher;8/surcoincher")
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

    val couleur = getCouleur
    if (couleur == 0) return Passe()
    // comment avoir le joueur qui a coinche quand il n'y a qu'un input ?
    if (couleur == 7) return Coinche(new Joueur(-1,""))
    if (couleur == 8) return SurCoinche(new Joueur(-1,""))
    Bid(couleur-1,getContrat,new Joueur(-1,""))

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

  /**
   *
   * @return le joueur qui a surcoinche, ou None si personne n'a coinche apres 5 secondes
   */
  def getSurCoinche: Option[Joueur] = None
}
