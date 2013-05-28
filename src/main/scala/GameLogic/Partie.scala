package GameLogic

import UI.{Printer, Reader}

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
  val listJoueur = List[Joueur](j1,j2,j3,j4)

  var deck = Deck.newShuffledDeck

  var dealer = j1
  var currentPlayer = j2

  var (scoreTotalEO,scoreTotalNS) = (0,0)

  // contient l'enchere courante
  var enchere:Enchere = new Enchere(0,0,0,0)

  def nextPlayer(j:Joueur):Joueur = j match {
    case `j1` => j2
    case `j2` => j3
    case `j3` => j4
    case `j4` => j1
  }

  //TODO
  /**
   *
   * @param couleurAtout couleur de l'atout
   * @return Score realise par NORD/SUD
   */
  def jouerLaMain(couleurAtout:Int):Int = {
    // La variable currentPlayer a ete modifie pendant les encheres
    // La variable dealer ne l'a pas ete
    var premierJoueur = nextPlayer(dealer)
    var tour = 1
    var scoreNS = 0

    //parce que desfois l'imperatif c'est quand meme pratique
    while (tour < 9) {
      currentPlayer = premierJoueur
      // la liste des cartes sur le pli
      var plis = List[(Joueur,Card)]()
      var couleurDemande:Option[Int] = None
      var plusFortAtout:Option[Card] = None
      var joueurMaitre = currentPlayer
      // cartemaitre et plusFortAtout pourrait etre regroupé
      // mais 1/ suppose de refactorise le code, donc fuck off
      //      2/ suppose de checker a chaque fois si carteMaitre est un atout
      var carteMaitre:Option[Card] = None


      // Tant que tout le monde n'a pas joué
      while (plis.length != 4){
        val (jouables,autres) = cartesJouables(currentPlayer.main,
                                            couleurDemande,
                                            couleurAtout,
                                            plusFortAtout,
                                            joueurMaitre)

        Printer.tourJoueur(currentPlayer)
        val carteJoue = Reader.getCard(jouables,autres)
        Printer.joueurAJoue(carteJoue)

        currentPlayer.main = currentPlayer.main.filterNot(_ == carteJoue)

        // Si c'est la premiere carte joue, renseigner la couleur demande
        if (couleurDemande.isEmpty) couleurDemande = Some(carteJoue.famille)

        // On met a jour la carte maitre et le joueur correspondant
        if (carteMaitre.isEmpty) carteMaitre = Some(carteJoue)
        else if (carteJoue.stronger(couleurAtout,carteMaitre.get).getOrElse(false)) {carteMaitre = Some(carteJoue);joueurMaitre = currentPlayer}

        // Si c'est un atout, on regarde s'il est meilleur
        if ((carteJoue.famille == couleurAtout) || (couleurDemande.getOrElse(-1) == carteJoue.famille && couleurAtout == 4)) {
          //premier atout
          if (plusFortAtout.isEmpty) plusFortAtout = Some(carteJoue)
          else if (carteJoue.stronger(couleurAtout,plusFortAtout.get).getOrElse(false)) plusFortAtout = Some(carteJoue)
        }

        plis = (currentPlayer,carteJoue)::plis
        currentPlayer = nextPlayer(currentPlayer)
      }

      premierJoueur = vainqueur(plis.reverse,couleurAtout)
      Printer.remporte(premierJoueur,plis.reverse)
      if (premierJoueur.id%2 == 0) scoreNS = scoreNS + countPoints(couleurAtout,plis.unzip._2)
      tour = tour + 1
    }
    // dix de der
    if (premierJoueur.id%2 == 0) scoreNS=scoreNS+10
    println(scoreNS)
    scoreNS
  }

  def countPoints(couleurAtout:Int,plis:List[Card]):Int = couleurAtout match{
    case 6 => plis.map(_.pointsSansAtout).sum
    case 5 => plis.map(_.pointsToutAtout).sum
    case a => plis.map({card => if (card.famille == couleurAtout) card.pointsAtout
                                else card.pointsClassique}).sum
  }

  def vainqueur(plis:List[(Joueur,Card)],couleurAtout:Int):Joueur = {
    var bestCard = plis.head._2
    var bestPlayer = plis.head._1

    plis.foreach({elem => {
      val (joueur,card) = elem
      // si les deux cartes ne sont pas comparables (stronger renvoie None)
      // bestCard gagne (soit la couleur demande, soit de l'atout)
      if (card.stronger(couleurAtout,bestCard).getOrElse(false)) {bestCard = card;bestPlayer = joueur}
      }
    }
    )
    bestPlayer
  }

  /**
   * @param main
   * @param couleurDemande
   * @param couleurAtout
   * @param plusFortAtout
   * @param joueurMaitre
   * @return Renvoie une paire de listes
   *         la premiere contient les cartes jouables,
   *         la deuxieme les cartes non-jouables
   */
  def cartesJouables(main:List[Card],
                     couleurDemande:Option[Int],
                     couleurAtout:Int,
                     plusFortAtout:Option[Card],
                     joueurMaitre:Joueur):(List[Card],List[Card]) =
    (couleurDemande,couleurAtout,plusFortAtout) match {
      //premier a jouer, on fait ce qu'on veux
      case (None,_,_) => (main,List[Card]())

      // On joue a sans atout
      // le 'None' en meilleur carte sur la table est obligé
      // sinon, il y a une incoherence
      // todo match sur le cas ou ce n'est pas none et gerer l'exception
      case (Some(couleurDemande),5,_) => {
        val cartesCouleurDemande = main.filter((_.famille == couleurDemande))
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else (cartesCouleurDemande,main.diff(cartesCouleurDemande))
      }

      // On joue a tout atout
      // le 'Some' en meilleur carte sur la table est obligé
      // sinon, il y a une incoherence
      // todo match sur le cas ou ce n'est pas Some et gerer l'exception
      case (Some(couleurDemande),4,Some(plusForte)) => {
        val cartesCouleurDemande = main.filter((_.famille == couleurDemande))
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else { // On doit monter, ....
          val plusHautes = cartesCouleurDemande.filter((_.pointsToutAtout > plusForte.pointsToutAtout))
          // sauf si on ne peut pas
          if (plusHautes.isEmpty) (cartesCouleurDemande,main.diff(cartesCouleurDemande))
          else (plusHautes,main.diff(plusHautes))
        }
      }

      // On joue la couleur de l'atout
      // les cartes jouables sont les memes quand tout atout
      // mais scala ne permet de matcher plusieurs possibilites en nommant les variables (ie couleurDemande)
      case (Some(couleurDemande),couleurAtout,Some(plusForte)) if (couleurDemande == couleurAtout) => {
        val cartesCouleurDemande = main.filter((_.famille == couleurDemande))
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else { // On doit monter, ....
        val plusHaute = cartesCouleurDemande.filter((_.pointsToutAtout > plusForte.pointsToutAtout))
          // sauf si on ne peut pas
          if (plusHaute.isEmpty) (cartesCouleurDemande,main.diff(cartesCouleurDemande))
          else (plusHaute,main.diff(plusHaute))
        }
      }

      // On joue une couleur autre que atout
      case (Some(couleurDemande),couleurAtout,Some(plusForte)) => {
        val cartesCouleurDemande = main.filter((_.famille == couleurDemande))
        // on a pas  la couleur demande
        if (cartesCouleurDemande.isEmpty) {
          // on doit couper, si on peut ou si le partenaire n'est pas maitre
          if (main.filter((_.famille == couleurAtout)).isEmpty || joueurMaitre.id == currentPlayer.idPartenaire) {
            // mais si on choisit de couper qd le part' est maitre, on doit monter
            val plusBasse = main.filter(_.famille == couleurAtout).filter(_.pointsAtout < plusForte.pointsAtout)
            (main.diff(plusBasse),plusBasse)
          }
          else {
            // on doit monter, si on peut
            val plusHaute = main.filter(_.famille == couleurAtout).filter(_.pointsAtout > plusForte.pointsAtout)
            if (plusHaute.isEmpty) main.partition(_.famille == couleurAtout)
            else (plusHaute,main.diff(plusHaute))
          }
        }
        // on joue la carte qu'on veut dans la famille demande
        else {
          (cartesCouleurDemande,main.diff(cartesCouleurDemande))
        }
      }

      // On joue une couleur autre que atout
      // et pas encore d'atout joue
      case (Some(couleurDemande),couleurAtout,None) => {
        val cartesCouleurDemande = main.filter((_.famille == couleurDemande))
        // on a pas  la couleur demande
        if (cartesCouleurDemande.isEmpty) {
          // on doit couper, si on peut
          if (main.filter((_.famille == couleurAtout)).isEmpty || joueurMaitre.id == currentPlayer.idPartenaire) (main,List[Card]())
          else main.partition(_.famille == couleurAtout)
        }
        // on joue la carte qu'on veut dans la famille demande
        else {
          (cartesCouleurDemande,main.diff(cartesCouleurDemande))
        }
      }
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
    else (162-score)<contrat
  }

  def start() {

    // reset des scores et du jeu
    deck = Deck.newShuffledDeck
    scoreTotalEO = 0;scoreTotalNS = 0

    while (scoreTotalEO < 1000 || scoreTotalNS < 1000){

      // on melange le jeu
      deck = Deck.shuffle(deck)
      // todo couper au hasard ?
      deck = Deck.coupe(deck,10).getOrElse(deck)

      // distribution des cartes
      val (m1,m2,m3,m4) = Deck.distribution(deck)
      dealer.main = Deck.trierMain(m4)
      nextPlayer(dealer).main = Deck.trierMain(m1)
      nextPlayer(nextPlayer(dealer)).main = Deck.trierMain(m2)
      nextPlayer(nextPlayer(nextPlayer(dealer))).main = Deck.trierMain(m3)
      //TODO trouver une facon moins moche de faire ca


      // boucle sur les encheres tant qu'il n'y en a pas
      def boucleEnchere():Enchere = {
        val e = Enchere.enchere()
        if (e.isEmpty) {
          Printer.pasDePrise()
          deck=Deck.shuffle(deck)
          boucleEnchere()
        }
        else e.get
      }
      enchere = boucleEnchere()

      //Les encheres sont finies, la main commence
      Printer.enchereFinie(enchere)
      val (couleur,contrat,equipe,coinche) = (enchere.couleur,enchere.contrat,enchere.id,enchere.coinche)
      listJoueur.foreach({joueur => joueur.main = Deck.trierMain(joueur.main,couleur)})

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

    //todo passer ca dans la partie UI
    // fin de la partie
    if (scoreTotalEO > 1000) println("GG Est Ouest")
    if (scoreTotalNS > 1000) println("GG Nord Sud")
  }
}
