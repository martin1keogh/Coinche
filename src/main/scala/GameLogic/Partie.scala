package GameLogic

import scala.util.Random
import UI.{Reader, Printer}

class Partie(val Printer:Printer,val Reader:Reader){

  val debug = true

  implicit val partie = this

  object State extends Enumeration {
    type State = Value
    val stopped = Value
    val running = Value
    val bidding = Value
    val playing = Value
  }

  case class Stopped() extends Exception

  // Game is not running from the start
  var state = State.stopped

  // Only print hand once (after trumps have been decided),
  // not every time you play a card.
  // Useful when I/O is a problem (IRC bot)
  var printOnlyOnce = false

  // the 4 players
  val (j1,j2,j3,j4) = (new Joueur(0,"Sud"),
                       new Joueur(1,"Ouest"),
                       new Joueur(2,"Nord"),
                       new Joueur(3,"Est"))
  // just for ease-of-use
  implicit val listJoueur = List[Joueur](j1,j2,j3,j4)

  val Deck = new Deck
  var deck = Deck.newShuffledDeck

  var dealer = j1
  implicit var currentPlayer = j2

  var (scoreTotalEO,scoreTotalNS) = (0,0)

  var capotChute = false
  var generalChute = false

  // Does someone has 'belote' ?
  var belote:Option[Joueur] = None

  val enchereController:EnchereController = new EnchereController

  def nextPlayer(j:Joueur):Joueur = j match {
    case `j1` => j2
    case `j2` => j3
    case `j3` => j4
    case `j4` => j1
  }

  def init():Unit = {
    state = State.stopped
    dealer = j1
    currentPlayer = j2
    scoreTotalEO = 0
    scoreTotalNS = 0
    listJoueur.zip(List[String]("Sud","Ouest","Nord","Est"))
              .foreach({case (j:Joueur,s:String) => j.rename(s)})
  }

  // checks if someone asked for the game to be stopped
  def checkStop() : Boolean = state == State.stopped

  def stopGame() : Unit = state = State.stopped

  /**
   *
   * @param couleurAtout
   * @param joueur
   * @return Returns true if 'joueur' has belote at 'couleurAtout'
   */
  def hasBelote(couleurAtout:Int,joueur:Joueur):Boolean = {
    val atouts = joueur.main.filter(_.famille == couleurAtout)
    // Has the queen               // Has the king
    atouts.exists(_.valeur == 4) && atouts.exists(_.valeur == 5)
  }

  /**
   *
   * @param couleurAtout couleur de l'atout
   * @return Score realise par NORD/SUD
   */
  def jouerLaMain(implicit couleurAtout:Int):Int = {
    // La variable currentPlayer a ete modifie pendant les encheres
    // La variable dealer ne l'a pas ete
    // Sur generale, le joueur prend la main
    var premierJoueur = if (enchereController.contrat == 400) listJoueur.find(_.id == enchereController.id).get
                        else nextPlayer(dealer)
    var tour = 1
    var scoreNS = 0
    capotChute = false; generalChute = false

    belote = listJoueur.find(hasBelote(couleurAtout,_))

    if (printOnlyOnce) Printer.printCardsToAll(couleurAtout)

    //parce que des fois l'imperatif c'est quand meme pratique
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
        if (checkStop()) throw Stopped()
        val (jouables,autres) = cartesJouables(currentPlayer.main,
                                               couleurDemande,
                                               couleurAtout,
                                               plusFortAtout,
                                               joueurMaitre)

        Printer.tourJoueur(currentPlayer)
        // state change before printCards, as the player may already know
        // which card he'll play
        state = State.playing
        if (!printOnlyOnce) Printer.printCards(jouables,autres)
        val carteJoue = Reader.getCard(jouables,autres)
        state = State.running
        Printer.joueurAJoue(carteJoue)
        // need to print 'belote' or 'rebelote'
        if (belote.exists(j => j.id == currentPlayer.id && j.id % 2 == enchereController.id % 2) // player has belote and his team won the bidding
            && carteJoue.famille == couleurAtout                                       // he plays a trump card
            && (carteJoue.valeur == 4 || carteJoue.valeur == 5)) {                     // which is the queen or the king
          Printer.annonceBelote(currentPlayer.main.filter(_.famille == couleurAtout).count(c => c.valeur == 4 || c.valeur == 5) == 2)
        }

        currentPlayer.main = currentPlayer.main diff List(carteJoue)

        // Si c'est la premiere carte joue, renseigner la couleur demande
        if (couleurDemande.isEmpty) couleurDemande = Some(carteJoue.famille)

        // On met a jour la carte maitre et le joueur correspondant
        if (carteMaitre.isEmpty) carteMaitre = Some(carteJoue)
        else if (carteJoue.stronger(couleurAtout,carteMaitre.get).getOrElse(false)) {carteMaitre = Some(carteJoue);joueurMaitre = currentPlayer}

        // Si c'est un atout, on regarde s'il est meilleur
        if ((carteJoue.famille == couleurAtout) || (couleurDemande.exists(_ == carteJoue.famille) && couleurAtout == 4)) {
          //premier atout
          if (plusFortAtout.isEmpty) plusFortAtout = Some(carteJoue)
          else if (carteJoue.stronger(couleurAtout,plusFortAtout.get).getOrElse(false)) plusFortAtout = Some(carteJoue)
        }

        plis = (currentPlayer,carteJoue)::plis
        currentPlayer = nextPlayer(currentPlayer)
      }

      premierJoueur = vainqueur(plis.reverse,couleurAtout)

      // on regarde si capot/general chute
      if (premierJoueur.id != enchereController.current.get.id) {
        generalChute = true
        if (premierJoueur.idPartenaire != enchereController.current.get.id) capotChute = true
      }

      Printer.remporte(premierJoueur,plis.reverse)
      if (premierJoueur.id%2 == 0) scoreNS = scoreNS + countPoints(couleurAtout,plis.unzip._2)
      tour = tour + 1
    }
    // dix de der
    if (premierJoueur.id%2 == 0) scoreNS+=10

    // belote
    if (belote.exists(_.id % 2 == enchereController.id % 2)) {
      if (enchereController.id % 2 == 0) scoreNS+=20 else scoreNS-=20
    }

    Printer.printScoreMain(scoreNS,enchereController.current.get,capotChute,generalChute)
    scoreNS
  }

  def countPoints(couleurAtout:Int,plis:List[Card]):Int = couleurAtout match{
    case 5 => plis.map(_.pointsSansAtout).sum
    case 4 => plis.map(_.pointsToutAtout).sum
    case a => plis.map({card => if (card.famille == couleurAtout) card.pointsAtout
                                else card.pointsClassique}).sum
  }

  def vainqueur(plis:List[(Joueur,Card)],couleurAtout:Int):Joueur = {
    // at first, the best card/player is the one who opened
    var bestCard = plis.head._2
    var bestPlayer = plis.head._1

    plis.foreach({elem => {
      val (joueur,card) = elem
      // si les deux cartes ne sont pas comparables (stronger renvoie None)
      // bestCard gagne (puisque soit la couleur demande, soit de l'atout)
      if (card.stronger(couleurAtout,bestCard).getOrElse(false)) {bestCard = card;bestPlayer = joueur}
      }
    }
    )
    bestPlayer
  }

  /**
   * @param main
   * @param couleurDemandeOption
   * @param couleurAtout
   * @param plusFortAtoutOption
   * @param joueurMaitre
   * @return Renvoie une paire de listes
   *         la premiere contient les cartes jouables,
   *         la deuxieme les cartes non-jouables
   */
  def cartesJouables(main:List[Card],
                     couleurDemandeOption:Option[Int],
                     couleurAtout:Int,
                     plusFortAtoutOption:Option[Card],
                     joueurMaitre:Joueur):(List[Card],List[Card]) =
    (couleurDemandeOption,plusFortAtoutOption) match {
      //premier a jouer, on fait ce qu'on veux
      case (None,_) => (main,List[Card]())

      // On joue a sans atout
      // le 'None' en meilleur carte sur la table est obligé
      // sinon, il y a une incoherence
      // todo match sur le cas ou ce n'est pas none et gerer l'exception
      case (Some(couleurDemande),_) if couleurAtout == 5 => {
        val cartesCouleurDemande = main.filter(_.famille == couleurDemande)
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else (cartesCouleurDemande,main.diff(cartesCouleurDemande))
      }

      // On joue a tout atout
      // le 'Some' en meilleur carte sur la table est obligé
      // sinon, il y a une incoherence
      // todo match sur le cas ou ce n'est pas Some et gerer l'exception
      case (Some(couleurDemande),Some(plusForte)) if couleurAtout == 4 => {
        val cartesCouleurDemande = main.filter(_.famille == couleurDemande)
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else { // On doit monter, ....
        val plusHautes = cartesCouleurDemande.filter(_.ordreAtout > plusForte.ordreAtout)
          // sauf si on ne peut pas
          if (plusHautes.isEmpty) (cartesCouleurDemande,main.diff(cartesCouleurDemande))
          else (plusHautes,main.diff(plusHautes))
        }
      }

      // On joue la couleur de l'atout
      case (Some(couleurDemande), Some(plusForte)) if couleurDemande == couleurAtout=> {
        val cartesCouleurDemande = main.filter(_.famille == couleurDemande)
        if (cartesCouleurDemande.isEmpty) // on a pas  la couleur demande
          (main,List[Card]())
        else { // On doit monter, ....
        val plusHaute = cartesCouleurDemande.filter(_.ordreAtout > plusForte.ordreAtout)
          // sauf si on ne peut pas
          if (plusHaute.isEmpty) (cartesCouleurDemande,main.diff(cartesCouleurDemande))
          else (plusHaute,main.diff(plusHaute))
        }
      }

      // On joue une couleur autre que atout et un atout a ete joue
      case (Some(couleurDemande), Some(plusForte)) => {
        val cartesCouleurDemande = main.filter(_.famille == couleurDemande)
        // on a pas  la couleur demande
        if (cartesCouleurDemande.isEmpty) {
          // si on a pas d'atout, on joue ce qu'on veut
          if (main.filter(_.famille == couleurAtout).isEmpty) (main,List())
          // si on ne peut pas monter
          else if (main.exists(c => c.famille == couleurAtout && c.ordreAtout < plusForte.ordreAtout)) {
            // si le part' est maitre, on joue ce qu'on veut
            if (joueurMaitre.id == currentPlayer.idPartenaire) (main,List())
            // sinon on doit sous-couper
            else main.partition(_.famille == couleurAtout)
          }
          // si on peut monter
          else {
            // si le part' est maitre, on peut tout jouer sauf des atouts plus faibles
            if (joueurMaitre.id == currentPlayer.idPartenaire) {
              val plusFaibles = main.filter(c => c.famille == couleurAtout && c.ordreAtout < plusForte.ordreAtout)
              (main diff plusFaibles,plusFaibles)
            }
            // sinon on doit sur-couper
            else main.partition(c => c.famille == couleurAtout && c.ordreAtout > plusForte.ordreAtout)
          }
        }
        // on joue la carte qu'on veut dans la famille demande
        else {
          (cartesCouleurDemande,main.diff(cartesCouleurDemande))
        }
      }

      // On joue une couleur autre que atout
      // et pas encore d'atout joue
      case (Some(couleurDemande), None) => {
        val cartesCouleurDemande = main.filter(_.famille == couleurDemande)
        // on a pas  la couleur demande
        if (cartesCouleurDemande.isEmpty) {
          // on doit couper, si on peut
          if (main.filter(_.famille == couleurAtout).isEmpty || joueurMaitre.id == currentPlayer.idPartenaire) (main,List[Card]())
          else main.partition(_.famille == couleurAtout)
        }
        // on joue la carte qu'on veut dans la famille demande
        else {
          (cartesCouleurDemande,main.diff(cartesCouleurDemande))
        }
      }
      case _ => println(couleurDemandeOption + " " + couleurAtout + " " + plusFortAtoutOption);(List[Card](),List[Card]())
    }


  /**
   *
   * @param contrat Le contrat a realise
   * @param score Le score fait par Nord/Sud
   * @param id 0 ou 2 si le contrat appartient a Nord/Sud, 1 ou 3 sinon
   * @return vrai si les points sont pour NS, faux sinon
   */
  def pointsPourNS(contrat: Int,score: Int, id: Int): Boolean = {
    if (id%2==0) {
      // N/S on pris
      if (contrat == 400) !generalChute
      else if (contrat == 250) !capotChute
      else score>contrat
    }
    else {
      if (contrat == 400) generalChute
      else if (contrat == 250) capotChute
      else (162-score)<contrat
    }
  }

  def start() {
    try {

    // reset des scores et du jeu
    deck = Deck.newShuffledDeck
    scoreTotalEO = 0;scoreTotalNS = 0
    state = State.running

    while (scoreTotalEO < 1001 && scoreTotalNS < 1001){

      // on melange le jeu
      deck = Deck.shuffle(deck)
      // nombre de carte coupé > 3 et < 29
      deck = Deck.coupe(deck,Random.nextInt(25)+4).getOrElse(deck)

      //distribution
      def distribute (deck:List[Card]) : Unit = {
        val mainList = Deck.distribution(deck).map(Deck.trierMain)
        nextPlayer(currentPlayer).main = mainList(0)
        nextPlayer(nextPlayer(currentPlayer)).main = mainList(1)
        nextPlayer(nextPlayer(nextPlayer(currentPlayer))).main = mainList(2)
        currentPlayer.main = mainList(3)
      }

      distribute(deck)

      // boucle sur les encheres tant qu'il n'y en a pas
      def boucleEnchere():Enchere = {
        Printer.printCardsToAll
        val e: Option[Enchere] = enchereController.enchere()
        if (e.isEmpty) {
          Printer.pasDePrise()
          deck=Deck.shuffle(deck)
          deck = Deck.coupe(deck,Random.nextInt(25)+4).getOrElse(deck)
          distribute(deck)
          boucleEnchere()
        }
        else e.get
      }
      val enchere = boucleEnchere()

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

      // On affiche les scores
      Printer.printScores(scoreTotalNS,scoreTotalEO)

      // on fait tourner les roles
      dealer = nextPlayer(dealer)
      currentPlayer = nextPlayer(dealer)
    }

    // fin de la partie
    Printer.printFin(scoreTotalNS,scoreTotalEO)
    init()
    } catch {
      case s : Stopped => init()
    }
  }
}
