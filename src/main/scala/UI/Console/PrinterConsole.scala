package UI.Console

import GameLogic._
import scala.collection.immutable.SortedMap
import UI.Printer
import GameLogic.Joueur

class PrinterConsole() extends Printer{
  def printFamille(famille:List[Card]) {
    val couleur = famille.head.familleToString
    val valeurs = famille.map(_.valeurToString).mkString(", ")
    println(couleur+" : "+valeurs)
  }

  def printCards(implicit joueur:Joueur) {
    println("----------------------------------")
    val listCartesParFamille = joueur.main.groupBy(_.famille)
    listCartesParFamille.foreach({case (_,famille) => printFamille(famille) })
    println("----------------------------------")
  }

  def printCards(couleurAtout:Int)(implicit joueur:Joueur) {
    println("----------------------------------")
    val listCartesParFamille = joueur.main.groupBy(_.famille)
    listCartesParFamille.foreach({case (_,famille) =>
      if (famille.head.famille == couleurAtout) printFamille(famille.sortBy(-_.pointsAtout))
      else printFamille(famille)
    })
    println("----------------------------------")
  }

  def printListEnchere(listEnchere:List[Enchere]) {
    println("----------------------------------")
    println("liste de precedentes annonces :")
    listEnchere.reverse.foreach(println(_))
    println()
    println("----------------------------------")
  }

  def printScores(NS:Int,EO:Int)(implicit listJoueur:List[Joueur]) {
    println("----------------------------------")
    println("Score Nord/Sud : "+NS)
    println("Score Est/Ouest : "+EO)
    println("----------------------------------")
  }


  def printCards(jouables:List[Card],autres:List[Card])(implicit joueur:Joueur,couleurAtout:Int) {
    println("----------------------------------")
    println("Jouables : ")
    //TRES SALE
    SortedMap(jouables.zipWithIndex.groupBy(_._1.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        if (l.head._1.famille == couleurAtout) print("(Atout) ") else print("        ")
        l.foreach({case (card:Card,index:Int) => print(index+"/"+card+"; ")});println()
      })
    println()
    if (!autres.isEmpty){
      println("Non Jouables : ")
      SortedMap(autres.groupBy(_.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        if (l.head.famille == couleurAtout) print("(Atout) ") else print("        ")
        l.foreach({case card:Card => print(card+"; ")});println()
      })
    }
    println("----------------------------------")
  }

  def tourJoueurEnchere(implicit j:Joueur) {
    println(">>>> A "+j+" de parler")
  }

  def joueurAJoue(c:Card)(implicit j:Joueur) {
    println(j+" a joué "+c)
    println()
  }

  def tourJoueur(implicit j:Joueur){
    println(">>>> A "+j+" de jouer (e pour voir l'enchere courante)")
  }

  def remporte(joueur:Joueur,plis:List[(Joueur,Card)]) {
    println(joueur+" remporte le pli avec : "+plis.find(_._1 == joueur).get._2)
    print("Pli : ")
    print("ouverture de "+plis.head._1+" au "+plis.head._2+", puis ")
    plis.tail.foreach({case (joueur,card) => print(joueur+" joue "+card+"; ")})
    println()
  }

  def printFin(NS:Int,EO:Int)(implicit listJoueur:List[Joueur]){
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
   * @param enchere EnchereController de la main
   */
  def printScoreMain(scoreNS: Int, enchere: Enchere,capotChute:Boolean,generaleChute:Boolean) {
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

  /**
   * Show everyone their hand.
   */
  def printCardsToAll(implicit listJoueur: List[Joueur]) {
    listJoueur.foreach(printCards(_))
  }

  def printCardsToAll(couleurAtout: Int)(implicit listJoueur: List[Joueur]) {
    listJoueur.foreach(printCards(couleurAtout)(_))
  }

  def printCoinche() {println("Coinche !")}

  /**
   *
   * @param first true if the first of the two cards (i.e, true if 'belote', false if 'rebelote')
   */
  def annonceBelote(first: Boolean)(implicit joueur: Joueur) {
    if (first) println(joueur+" annonce Belote.")
    else println(joueur+" annonce rebelote.")
  }
}
