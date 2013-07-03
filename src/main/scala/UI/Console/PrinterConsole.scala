package UI.Console

import GameLogic.{Joueur, Enchere, Partie, Card}
import scala.collection.immutable.SortedMap
import UI.Printer

object PrinterConsole extends Printer{
  def printSmth(s:String) {
    s match {
      case "h" => printHelp()
      case "l" => printListEnchere()
      case "s" => printScores()
      case "c" => printCartes()
      case _ => ()
    }
  }

  def printCardsToAll() {}

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


  def printCartes(jouables:List[Card],autres:List[Card]) {
    println("----------------------------------")
    println("Jouables : ")
    //TRES SALE
    SortedMap(jouables.zipWithIndex.groupBy(_._1.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        if (l.head._1.famille == Partie.enchere.couleur) print("(Atout) ") else print("        ")
        l.foreach({case (card:Card,index:Int) => print(index+"/"+card+"; ")});println()
      })
    println()
    if (!autres.isEmpty){
      println("Non Jouables : ")
      SortedMap(autres.groupBy(_.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        if (l.head.famille == Partie.enchere.couleur) print("(Atout) ") else print("        ")
        l.foreach({case card:Card => print(card+"; ")});println()
      })
    }
    println("----------------------------------")
  }

  def tourJoueurEnchere(j:Joueur) {
    println(">>>> A "+j+" de parler")
  }

  def joueurAJoue(c:Card) {
    println(Partie.currentPlayer+" a joué "+c)
    println()
  }

  def printEnchere(){
    println("Enchere courante : "+Partie.enchere)
  }

  def tourJoueur(j:Joueur){
    println(">>>> A "+j+" de jouer (e pour voir l'enchere courante)")
  }

  def remporte(joueur:Joueur,plis:List[(Joueur,Card)]) {
    println(joueur+" remporte le pli avec : "+plis.find(_._1 == joueur).get._2)
    print("Pli : ")
    print("ouverture de "+plis.head._1+" au "+plis.head._2+", puis ")
    plis.tail.foreach({case (joueur,card) => print(joueur+" joue "+card+"; ")})
    println()
  }

  def printFin(NS:Int,EO:Int){
    println("Score de Nord/Sud : "+NS)
    println("Score de ESt/Ouest : "+EO)
    if (NS>EO) println("Nord/Sud gagnent !")
    else println("Est/Ouest gagnent !")
  }

  def pasDePrise() {
    println("Pas de Prise !")
    println()
  }

  def enchereFinie(e:Enchere) {
    println("--------------Fin des encheres ----------------")
    println(e)
    println("-----------------------------------------------")
    println()
  }

  /**
   * Affiche le nombre de points fait par chaque equipe, si la donne est chutee, etc
   *
   * @param scoreNS Nombre de points fait par Nord/Sud durant cette main
   * @param enchere Enchere de la main
   */
  def printScoreMain(scoreNS: Int, enchere: Enchere) {
    println("Contrat : "+enchere.toString)
    if (enchere.id % 2 == 0) {
      if (scoreNS >= enchere.contrat) {println("Passe de "+(scoreNS - enchere.contrat))}
      else {println("Chute de "+(enchere.contrat - scoreNS))}
    } else {
      val scoreEO = 162 - scoreNS
      if (scoreEO >= enchere.contrat) {println("Passe de "+(scoreEO - enchere.contrat))}
      else {println("Chute de "+(enchere.contrat - scoreEO))}
    }
  }

  def printCardsToAll(couleurAtout: Int) {}

  def printCoinche() : Unit = {
    println("5 secondes pour sur-coinché !")
  }
}
