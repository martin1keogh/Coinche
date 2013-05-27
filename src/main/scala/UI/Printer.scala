package UI

import GameLogic.{Joueur, Enchere, Partie, Card}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 27/05/13
 * Time: 02:16
 * To change this template use File | Settings | File Templates.
 */
object Printer {
  def printSmth(s:String) {
    s match {
      case "h" => printHelp()
      case "l" => printListEnchere()
      case "s" => printScores()
      case "c" => printCartes()
      case _ => ()
    }
  }

  def printFamille(famille:List[Card]) {
    val couleur = famille.head.familleToString
    val valeurs = famille.map(_.valeurToString).mkString(", ")
    println(couleur+" : "+valeurs)
  }

  def printCartes() {
    println("----------------------------------")
    val main = Partie.currentPlayer.main
    val listCartesParFamille = main.groupBy(_.famille)
    listCartesParFamille.foreach({famille =>
      printFamille(famille._2)
    })
    println("----------------------------------")

  }

  def printListEnchere() {
    println("----------------------------------")
    println("liste de precedentes annonces :")
    Enchere.listEnchere.reverse.foreach(println(_))
    println()
    println("----------------------------------")
  }

  def printHelp() {
    println("----------------------------------")
    println("Aide de jeu :")
    println("l/ liste des precedentes encheres")
    println("h/ afficher cette aide")
    println("s/ voir les scores")
    println("c/ voir ses cartes")
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
