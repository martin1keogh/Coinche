package UI

import GameLogic.{Joueur, Enchere, Card}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 02/06/13
 * Time: 19:03
 * To change this template use File | Settings | File Templates.
 */
trait Printer {

  /**
   * Affiche les cartes du joueur, trier par famille.
   * Utiliser uniquement durant les encheres.
   */
  def printCartes()

  /**
   * Affiche toutes les encheres effectuees durant ce tour d'annonces
   */
  def printListEnchere()

  /*
  /**
   * Affiche les differentes commandes accessibles, ainsi que leur but
   * Peut-etre inutile avec une vrai interface graphique
   */
  def printHelp() {}
  */

  /**
   * Affiche les scores (ou les met a jour pour une GUI).
   * Appelée a chaque fin de main.
   */
  def printScores()

  /**
   * Affiche les cartes de la main du joueur.
   * Appelée a chaque tour du joueur.
   * @param jouables cartes jouables en fonction des cartes deja jouees
   * @param autres cartes non jouables
   */
  def printCartes(jouables:List[Card],autres:List[Card])

  /**
   * Affiche "A X de parler"
   * @param joueur
   */
  def tourJoueurEnchere(joueur:Joueur)

  /**
   * Affiche "A X de jouer"
   * @param j
   */
  def tourJoueur(j:Joueur)

  /*
  /**
   * Affiche l'enchere courante.
   * Peut-etre inutile avec une vrai interface graphique (si l'enchere est toujours affichée).
   */
  def printEnchere() {}
  */

  /**
   * Affiche la carte joue.
   * @param c la carte joue
   */
  def joueurAJoue(c:Card)

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
  def printFin(NS:Int,EO:Int)

  /**
   * Affiche "pas de prise"
   */
  def pasDePrise()

  /**
   * Annonce la fin des encheres, et affiche l'enchere gagnante
   * @param e enchere gagnante.
   */
  def enchereFinie(e:Enchere)

}
