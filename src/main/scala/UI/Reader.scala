package UI

import GameLogic.{Partie, Joueur, Enchere}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 22:13
 * To change this template use File | Settings | File Templates.
 */
object Reader {

  def getCouleur:Int = {
    var couleur = -1
    do {
      println("Quelle couleur (entree pour passer, h pour l'aide) ?")
      println("1/Pique;2/Carreau;3/Trefle;4/Coeur;5/Tout atout;6/Sans Atout")
      couleur = try {
        val c = readLine()
        printSmth(c)
        if (c == "") 0 // tres sale,permet de gerer le <entree> pour passer son tour
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
    printSmth(c)
    try {c.toInt}
    catch {case e:NumberFormatException => -1}
  }

  def printSmth(s:String) {
    s match {
      case "h" => printHelp()
      case "l" => printListEnchere()
      case "s" => printScores()
      case _ => ()
    }
  }

  def printListEnchere() {
    println("----------------------------------")
    println("liste de precedentes annonces :")
    Enchere.listEnchere.foreach(println(_))
    println()
    println("----------------------------------")
  }

  def printHelp() {
    println("----------------------------------")
    println("Aide de jeu :")
    println("l/ liste des precedentes encheres")
    println("h/ afficher cette aide")
    println("s/ voir les scores")
    println("----------------------------------")
  }

  def printScores() {
    println("----------------------------------")
    println("Score Nord/Sud : "+Partie.scoreTotalNS)
    println("Score Est/Ouest : "+Partie.scoreTotalEO)
    println("----------------------------------")
  }

  def tourJoueurEnchere(j:Joueur) {
    println(">>>> A "+j+" de parler")
  }

  def pasDePrise() {
    println("Pas de Prise !")
  }

  def enchereFinie(e:Enchere) {
    println("--------------Fin des encheres ----------------")
    println(e)
    println("-----------------------------------------------")
  }

}
