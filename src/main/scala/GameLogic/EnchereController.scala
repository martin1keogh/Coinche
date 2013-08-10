package GameLogic

import GameLogic.Enchere._
import akka.pattern.ask
import UI.Router.{ReturnResults, AwaitSurCoinche, Normal, AwaitBid}
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.util.Timeout
import scala.language.postfixOps
import GameLogic.Bot.BotTrait

class EnchereController(implicit Partie:Partie){

  import UI.Reader._

  implicit val timeout = new Timeout(10 minutes)

  val PlayerTypeChangeException:Exception = new Exception

  val Router = Partie.Reader.router

  var listEnchere:List[Enchere] = List()
  var current:Option[Enchere] = None

  val enchereNull = new Enchere(Undef,70,-1,"",-1)
  def couleur = current.getOrElse(enchereNull).couleur
  def contrat = current.getOrElse(enchereNull).contrat
  def id = current.getOrElse(enchereNull).id
  def coinche = current.getOrElse(enchereNull).coinche

  def annonceLegal(j:Joueur,a:Int):Boolean = {
    a>contrat && ( a == 250 || a == 400 || (a%10 == 0 && a < 170)) && Partie.currentPlayer == j && coinche < 2
  }

  def passeLegal(j:Joueur):Boolean = {
    Partie.currentPlayer == j && coinche < 2
  }

  def coincheValid(j:Joueur) = contrat > 80 && id % 2 != j.id % 2

  def surCoincheValid(j:Joueur) = coinche == 2 && id % 2 == j.id % 2

  def enchereCoinche(e:Enchere):Enchere = {val ret = e;ret.coinche = 2;ret}

  def enchereSurCoinche(e:Enchere):Enchere = {val ret = e;ret.coinche = 4;ret}

  def effectuerEnchere():Option[Enchere] = {
    def readMessage:Option[Enchere] = {
      val card = try {Await.result(Router ? AwaitBid,Duration.Inf)}
                 catch {case t:java.util.concurrent.TimeoutException => {Router ! StopWaiting; None}}
      card match {
        case Coinche(j) if coincheValid(j) => Some(enchereCoinche(current.get))
        case SurCoinche(j) if surCoincheValid(j) => Some(enchereSurCoinche(current.get))
        case Passe(j) if passeLegal(j) => None
        case Bid(j,couleur,valeur) if annonceLegal(j,valeur) => Some(new Enchere(couleur,valeur,j.id,j.nom))
        case Bid(j,_,_) if j == Partie.currentPlayer => {Partie.Printer.annonceImpossible;readMessage}
        case StopGame => throw new InterruptedException
        case PlayerTypeChange => throw PlayerTypeChangeException
        case _ => readMessage
      }
    }
    readMessage
  }

  def getSurCoinche:Option[Enchere] = {
    Router ! AwaitSurCoinche
    Thread.sleep(5000) // 5 secondes pour surcoincher
    val listSurCoinche = Await.result((Router ? ReturnResults).mapTo[List[Joueur]], 10 seconds)
    if (listSurCoinche.exists(surCoincheValid)) {
      val surCoinche = enchereSurCoinche(current.get)
      listEnchere = surCoinche :: listEnchere
      Some(enchereSurCoinche(current.get))
    } else None
  }

  /**
   *
   * @return Option sur enchere : (couleur,contrat,id,coinche)
   *         id : 0 pour sud, 1 pour ouest, 2 pour nord, 3 pour est
   *         coinche : 1 = pas de coinche, 2 = coinché, 4 = contré
   */
  def enchere():Option[Enchere] = {


    // On reinitialise les variables globales
    current = None
    var nbPasse = 0
    listEnchere = List()

    Partie.state = Partie.State.bidding


    // Boucle principale lors des encheres
    while ( (nbPasse < 3)                      // apres 3 passes on finit les encheres
      || (current == None && nbPasse == 3)){   // sauf s'il n'y a pas eu d'annonce,auquel cas on attend le dernier joueur
      if (current.exists(_.coinche > 1)) {
        Partie.Printer.printCoinche()
        nbPasse = 4
        current = getSurCoinche orElse current
      } else {
        Partie.Printer.tourJoueurEnchere(Partie.currentPlayer)
        // Receiving a PlayerTypeChangeException means a human player was replaced by a bot
        // We stop waiting for input and then asks the bot for a bid
        def getEnchere: Option[Enchere] = try {
          Partie.currentPlayer match {
            case b:BotTrait => b.effectuerEnchere(listEnchere)
            case j:Joueur => effectuerEnchere()
          }
        } catch {case `PlayerTypeChangeException` => getEnchere}
        val enchere = getEnchere
        if (enchere.isEmpty) nbPasse=nbPasse+1
        else {
          //une enchere a etait faite, on remet le nombre de passe a zero
          nbPasse=0
          listEnchere = enchere.get :: listEnchere
          current = enchere
        }
        Partie.Printer.printEnchere(enchere)
        Partie.currentPlayer = Partie.nextPlayer(Partie.currentPlayer)
      }
    }

    Router ! Normal
    Partie.state = Partie.State.running

    current
  }
}
