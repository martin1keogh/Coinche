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

  //TODO
  /**
   *
   * @param couleur couleur de l'atout
   * @return Score realise par NORD/SUD
   */
  def jouerLaMain(couleur:Int):Int = {
    0
  }

  //TODO
  /**
   *
   * @return (couleur,score,equipe,coinche)
   *         equipe : true pour NS, false pour EO
   *         coinche : 1 = pas de coinche, 2 = coinche, 4 = contre
   */
  def enchere():(Int,Int,Boolean,Int) = {
    //TODO
    // Penser a boucler tant qu'il n'y a pas d'enchere
    // Penser a melanger le jeu
    // Penser a changer le dealer
    (80,0,true,0)
  }

  /**
   *
   * @param contrat Le contrat a realise
   * @param score Le score fait par Nord/Sud
   * @param prisParNS true si le contrat appartient a Nord/Sud
   * @return
   */
  def pointsPourNS(contrat: Int,score: Int, prisParNS: Boolean): Boolean = {
    if (prisParNS) score>contrat
    else score<contrat
  }

  def start() {
    // reset des scores et du jeu
    deck = Deck.newShuffledDeck
    var (scoreTotalEO,scoreTotalNS) = (0,0)

    while (scoreTotalEO < 1000 || scoreTotalNS < 1000){

      val (couleur,contrat,prisParNS,coinche) = enchere()

      val scoreFaitParNS = jouerLaMain(couleur)

      // scores update

      if (pointsPourNS(contrat,scoreFaitParNS,prisParNS)) {
        scoreTotalNS=scoreTotalNS+(contrat*coinche)
      }
      else scoreTotalEO=scoreTotalEO+(contrat*coinche)
    }

    // fin de la partie
    if (scoreTotalEO > 1000) println("GG Est Ouest")
    if (scoreTotalNS > 1000) println("GG Nord Sud")
  }
}
