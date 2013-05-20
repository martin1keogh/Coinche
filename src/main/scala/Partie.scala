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

  //TODO gerer les coinches
  /**
   *
   * @return (couleur,score,equipe,coinche)
   *         equipe : symbol 'NS ou 'EO
   *         coinche : 1 = pas de coinche, 2 = coinche, 4 = contre
   */
  def enchere():Option[(Int,Int,Symbol,Int)] = {

    var ret:Option[(Int,Int,Symbol,Int)] = None
    var nbPasse = 0

    def annonceLegal(a:Int):Boolean = {
      val annonceCourante = ret.getOrElse({(0,0,'FU,0)})._2
      //TODO gerer les capots/generales
      (a%10 == 0 && a > annonceCourante && a < 170)
    }

    def annonceImpossible():Boolean = {
      if (ret.isEmpty) true
      else ret.get._2 >= 160
    }

    //TODO
    //TODO doit regarder que l'enchere est legale
    // renvoie : (couleur,score)
    def effectuerEnchere():Option[(Int,Int)] = None


    while ( (ret == None && nbPasse < 4) || // tant qu'il n'y pas eu d'enchere, on attend les 4 passes
            (nbPasse < 3) ||                // apres, 3 passes finissent les encheres
            (!annonceImpossible())          // ou l'impossibilite de monter
          ){
      val enchere = effectuerEnchere()
      if (enchere.isEmpty) nbPasse=nbPasse+1
      else {
        //une enchere a etait faite, on remet le nombre de passe a zero
        nbPasse=0
        ret = Some(enchere.get._1,enchere.get._2,currentPlayer.Equipe,1)
      }
      currentPlayer = nextPlayer(currentPlayer)
    }

    ret
  }

  /**
   *
   * @param contrat Le contrat a realise
   * @param score Le score fait par Nord/Sud
   * @param prisParNS true si le contrat appartient a Nord/Sud
   * @return
   */
  def pointsPourNS(contrat: Int,score: Int, prisParNS: Symbol): Boolean = {
    if (prisParNS == 'NS) score>contrat
    else score<contrat
  }

  def start() {

    // reset des scores et du jeu
    deck = Deck.newShuffledDeck
    var (scoreTotalEO,scoreTotalNS) = (0,0)

    while (scoreTotalEO < 1000 || scoreTotalNS < 1000){

      // Penser a boucler tant qu'il n'y a pas d'enchere
      // todo actually test it
      val (couleur:Int,contrat:Int,prisParNS:Symbol,coinche:Int) = enchere().getOrElse({enchere()})

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
