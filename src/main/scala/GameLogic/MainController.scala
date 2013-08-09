package GameLogic

import scala.concurrent.Await
import UI.Router.AwaitCard
import akka.pattern.ask
import UI.Reader.{StopGame, PlayCard, PlayingMessage, StopWaiting}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps

class MainController(implicit Partie:Partie) {

  import Partie._

  val Router = Reader.router

  implicit val timeout = new Timeout(10 minutes)

  /**
   *
   * @param couleurAtout couleur de l'atout durant cette main
   * @param joueur
   * @return Returns true if 'joueur' has belote at 'couleurAtout'
   */
  def hasBelote(couleurAtout:Int,joueur:Joueur):Boolean = {
    val atouts = joueur.main.filter(_.famille == couleurAtout)
    // Has the queen               // Has the king
    atouts.exists(_.valeur == 4) && atouts.exists(_.valeur == 5)
  }

  def getCard(jouables:List[Card],autres:List[Card]):Card = {
    // play automatically the last card
    if (jouables.length == 1 && autres.isEmpty) jouables(0)
    else {
      val cardList = try {Await.result(Router ? AwaitCard,Duration.Inf)}
                     catch {case t:java.util.concurrent.TimeoutException => {Router ! StopWaiting; None}}
      cardList match {
        case PlayCard(joueur,card) if joueur == currentPlayer => {
          val c = jouables.find(c => card.exists(cc => cc.equals(c)))
          c.getOrElse({Printer.cardUnplayable;getCard(jouables,autres)})
        }
        case StopGame => throw new InterruptedException
        case e => getCard(jouables,autres)
      }
    }
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
        val carteJoue = getCard(jouables,autres)
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
   * @param main Liste contenant les cartes du joueurs
   * @param couleurDemandeOption Couleur de la premiere carte joué durant ce pli (None si il n'y a pas encore de carte jouée)
   * @param couleurAtout Couleur de l'atout durant cette main
   * @param plusFortAtoutOption Plus fort atout sur la table (None s'il n'y a aucun atout sur la table)
   * @param joueurMaitre Joueur controllant le pli
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


}
