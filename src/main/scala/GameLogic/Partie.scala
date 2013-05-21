package GameLogic

import GameLogic.{Joueur, Enchere, Deck}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 17:16
 * To change this template use File | Settings | File Templates.
 */

object Partie {

  val (j1,j2,j3,j4) = (new Joueur(0),
                       new Joueur(1),
                       new Joueur(2),
                       new Joueur(3))

  var deck = Deck.newShuffledDeck

  var dealer = j1
  var currentPlayer = j2

  def nextPlayer(j:Joueur):Joueur = j match {
    case `j1` => j2
    case `j2` => j3
    case `j3` => j4
    case `j4` => j1
  }

  //TODO
  /**
   *
   * @param couleur couleur de l'atout
   * @return Score realise par NORD/SUD
   */
  def jouerLaMain(couleur:Int):Int = {
    0
  }

  /**
   *
   * @param contrat Le contrat a realise
   * @param score Le score fait par Nord/Sud
   * @param id 0 ou 2 si le contrat appartient a Nord/Sud
   * @return
   */
  def pointsPourNS(contrat: Int,score: Int, id: Int): Boolean = {
    if (id%2==0) score>contrat
    else score<contrat
  }

  def start() {

    // reset des scores et du jeu
    deck = Deck.newShuffledDeck
    var (scoreTotalEO,scoreTotalNS) = (0,0)

    while (scoreTotalEO < 1000 || scoreTotalNS < 1000){

      // boucle sur les encheres tant qu'il n'y en a pas
      def boucleEnchere():Enchere = {
        val e = Enchere.enchere()
        if (e.isEmpty) {
          Reader.pasDePrise()
          deck=Deck.shuffle(deck)
          boucleEnchere()
        }
        else e.get
      }
      val e = boucleEnchere()
      Reader.enchereFinie(e)
      val (couleur,contrat,equipe,coinche) = (e.couleur,e.contrat,e.id,e.coinche)

      val scoreFaitParNS = jouerLaMain(couleur)

      // scores update

      if (pointsPourNS(contrat,scoreFaitParNS,equipe)) {
        scoreTotalNS=scoreTotalNS+(contrat*coinche)
      }
      else scoreTotalEO=scoreTotalEO+(contrat*coinche)

      // on fait tourner les roles
      dealer = nextPlayer(dealer)
      currentPlayer = nextPlayer(dealer)
    }

    // fin de la partie
    if (scoreTotalEO > 1000) println("GG Est Ouest")
    if (scoreTotalNS > 1000) println("GG Nord Sud")
  }
}
