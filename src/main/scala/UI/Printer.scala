package UI

import GameLogic._
import GameLogic.Joueur

trait Printer {

  def printCards(implicit joueur:Joueur)

  def printCards(couleurAtout:Int)(implicit joueur:Joueur)

  def printEnchere(enchere:Option[Enchere])

  /**
   * Affiche toutes les encheres effectuees durant ce tour d'annonces
   */
  def printListEnchere(listEnchere:List[Enchere])

  /**
   * Show everyone their hand.
   */
  def printCardsToAll(implicit listJoueur:List[Joueur])

  def printCardsToAll(couleurAtout:Int)(implicit listJoueur:List[Joueur])

  /**
   * Affiche les scores (ou les met a jour pour une GUI).
   * Appelée a chaque fin de main.
   */
  def printScores(NS:Int,EO:Int)(implicit listJoueur:List[Joueur])

  /**
   * Affiche le nombre de points fait par chaque equipe, si la donne est chutee, etc
   *
   * @param scoreNS Nombre de points fait par Nord/Sud durant cette main
   * @param enchere Enchere de la main
   * @param capotChute true si un capot a ete annoncé et chuté
   * @param generaleChute true si une generale a ete annoncée et chutée
   */
  def printScoreMain(scoreNS:Int,enchere:Enchere,capotChute:Boolean,generaleChute:Boolean)

  /**
   * Affiche les cartes de la main du joueur.
   * Appelée a chaque tour du joueur.
   * @param jouables cartes jouables en fonction des cartes deja jouees
   * @param autres cartes non jouables
   */
  def printCards(jouables:List[Card],autres:List[Card])(implicit joueur:Joueur, couleurAtout:Int)

  /**
   * Affiche "A X de parler"
   * @param joueur
   */
  def tourJoueurEnchere(implicit joueur:Joueur)

  def cardUnplayable

  def annonceImpossible

  /**
   * Affiche "A X de jouer"
   * @param j
   */
  def tourJoueur(implicit j:Joueur)

  /**
   * Affiche la carte joue.
   * @param c la carte joue
   */
  def joueurAJoue(c:Card)(implicit j:Joueur)

  /**
   * Signale quel joueur a remporte le pli, et comment.
   * @param joueur le joueur ayant remporte le pli
   * @param plis List de type (Joueur,Card)
   *             Represente les cartes jouées, dans l'ordre (FIFO).
   */
  def remporte(joueur:Joueur,plis:List[(Joueur,Card)])

  /**
   * Affiche qui a gagner, les scores
   * @param NS score Nord/Sud
   * @param EO score Est/Ouest
   */
  def printFin(NS:Int,EO:Int)(implicit listJoueur:List[Joueur])

  /**
   * Affiche "pas de prise"
   */
  def pasDePrise()

  /**
   *
   * @param first true if the first of the two cards (i.e, true if 'belote', false if 'rebelote')
   */
  def annonceBelote(first:Boolean)(implicit joueur:Joueur):Unit

  /**
   * Annonce la fin des encheres, et affiche l'enchere gagnante
   * @param e enchere gagnante.
   */
  def enchereFinie(e:Enchere)

  /**
   * Prints '5 more secs to sur-coinche'
   */
  def printCoinche() : Unit

}
