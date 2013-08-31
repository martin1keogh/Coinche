package GameLogic

import scala.util.Random
import UI.{Reader, Printer}
import UI.Reader.PlayerTypeChange
import GameLogic.Bot.BotTrait
import Joueur._


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
  var (j1,j2,j3,j4) = (new Joueur(Sud,"Sud"),
                       new Joueur(Ouest,"Ouest"),
                       new Joueur(Nord,"Nord"),
                       new Joueur(Est,"Est"))
  // just for ease-of-use
  implicit def listJoueur = List[Joueur](j1,j2,j3,j4)

  def updateRef(old:Joueur,_new:Joueur):Unit = {
    if (currentPlayer == old) currentPlayer = _new
    if (dealer == old) dealer = _new
  }

  def playerToBot(old:Joueur,_new:BotTrait):Unit = {
    old match{
      case j if j == j1 => j1 = _new
      case j if j == j2 => j2 = _new
      case j if j == j3 => j3 = _new
      case j if j == j4 => j4 = _new
    }
    _new.main = old.main
    updateRef(old,_new)
    Reader.router ! PlayerTypeChange
  }

  def botToPlayer(old:BotTrait): Unit = {
    def createPlayer(b:Joueur) = {
      val j = new Joueur(b.id,b.nom)
      j.main = b.main
      j
    }
    val _new = createPlayer(old)
    old match {
      case j if j == j1 => j1 = _new
      case j if j == j2 => j2 = _new
      case j if j == j3 => j3 = _new
      case j if j == j4 => j4 = _new
    }
    updateRef(old,_new)
    Reader.router ! PlayerTypeChange
  }

  val Deck = new Deck
  var deck = Deck.newShuffledDeck

  var dealer = j1
  implicit var currentPlayer = j2

  var (scoreTotalEO,scoreTotalNS) = (0,0)

  var capotChute = false
  var generalChute = false

  // Does someone has 'belote' ?
  var belote:Option[Joueur] = None

  lazy val enchereController = new EnchereController
  lazy val mainController = new MainController

  def nextPlayer(j:Joueur):Joueur = listJoueur.find(next => next.id == (j.id match {
    case Sud => Ouest
    case Ouest => Nord
    case Nord => Est
    case Est => Sud
  })).get

  def init():Unit = {
    state = State.stopped
    dealer = j1
    currentPlayer = j2
    scoreTotalEO = 0
    scoreTotalNS = 0
    listJoueur.zip(List[String]("Sud","Ouest","Nord","Est")).foreach({case (j:Joueur,s:String) => j.rename(s)})
    // remove all bots
    listJoueur.foreach({case b:BotTrait => botToPlayer(b);case _:Joueur => ()})
  }

  def distribute (deck:List[Card]) : Unit = {
    val mainList = Deck.distribution(deck).map(Deck.trierMain)
    nextPlayer(currentPlayer).main = mainList(0)
    nextPlayer(nextPlayer(currentPlayer)).main = mainList(1)
    nextPlayer(nextPlayer(nextPlayer(currentPlayer))).main = mainList(2)
    currentPlayer.main = mainList(3)
  }

  // boucle sur les encheres tant qu'il n'y en a pas
  def getEnchere():Enchere = {
    Printer.printCardsToAll
    val e: Option[Enchere] = enchereController.enchere()
    if (e.isEmpty) {
      Printer.pasDePrise()
      deck = Deck.shuffle(deck)
      deck = Deck.coupe(deck,Random.nextInt(25)+4).getOrElse(deck)
      distribute(deck)
      getEnchere()
    }
    else e.get
  }


  // checks if someone asked for the game to be stopped
  def checkStop() : Boolean = state == State.stopped

  def stopGame() : Unit = state = State.stopped

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
      else score>=contrat
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
    currentPlayer = nextPlayer(dealer)

    while (scoreTotalEO < 1001 && scoreTotalNS < 1001){
      // on melange le jeu
      deck = Deck.shuffle(deck)
      // nombre de carte coupé > 3 et < 29
      deck = Deck.coupe(deck,Random.nextInt(25)+4).getOrElse(deck)

      //distribution
      distribute(deck)

      val enchere = getEnchere()

      //Les encheres sont finies, la main commence
      Printer.enchereFinie(enchere)
      val (couleur,contrat,equipe,coinche) = (enchere.couleur,enchere.contrat,enchere.id,enchere.coinche)
      listJoueur.foreach({joueur => joueur.main = Deck.trierMain(joueur.main,couleur)})

      val scoreFaitParNS = mainController.jouerLaMain(couleur)

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
    } catch{
      case e : InterruptedException => init()
    }
  }
}
