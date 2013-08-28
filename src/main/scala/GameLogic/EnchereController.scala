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
  def current:Option[Enchere] = listEnchere.headOption

  val enchereNull = new Enchere(Undef,70,-1,"",-1)
  def couleur = current.getOrElse(enchereNull).couleur
  def contrat = current.getOrElse(enchereNull).contrat
  def id = current.getOrElse(enchereNull).id
  def coinche = current.getOrElse(enchereNull).coinche

  /**
   *
   * @param joueur Joueur qui a annoncé
   * @param valeur Valeur de l'annonce
   * @return true si : c'est a joueur de parler
   *                   valeur est superieur a l'enchere courante
   *                   valeur est legal (80,...,160,250 ou 400)
   *                   l'enchere courante n'est pas coinché
   *         false sinon
   */
  def annonceLegal(joueur:Joueur,valeur:Int):Boolean = {
    valeur>contrat && ( valeur== 250 || valeur== 400 || (valeur%10 == 0 && valeur< 170 && valeur > 70)) &&
    Partie.currentPlayer == joueur && coinche < 2
  }

  /**
   *
   * @param j le joueur qui passe
   * @return true si : c'est a j de parler
   *                   l'enchere courante n'est pas coinche
   */
  def passeLegal(j:Joueur):Boolean = {
    Partie.currentPlayer == j && coinche < 2
  }

  /**
   *
   * @param j le joueur qui a coinché
   * @return true si : j n'est pas dans l'equipe qui tient l'enchere courante
   *                   l'enchere courante est superieur a 80
   *                   l'enchere courante n'a pas deja ete coinche
   */
  def coincheValid(j:Joueur) = contrat > 80 && id % 2 != j.id % 2 && coinche == 1

  /**
   *
   * @param j le joueur qui a surcoinche
   * @return true si : j est dans l'equipe de l'enchere courante
   *                   l'enchere courante a etait coinche
   */
  def surCoincheValid(j:Joueur) = coinche == 2 && id % 2 == j.id % 2

  def enchereCoinche(e:Enchere):Enchere = e.copy(coinche = 2)

  def enchereSurCoinche(e:Enchere):Enchere = e.copy(coinche = 4)

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
    Partie.Printer.printCoinche()
    Router ! AwaitSurCoinche
    Thread.sleep(5000) // 5 secondes pour surcoincher
    val listSurCoinche = Await.result((Router ? ReturnResults).mapTo[List[Joueur]], 10 seconds)
    if (listSurCoinche.exists(surCoincheValid)) {
      val surCoinche = enchereSurCoinche(current.get)
      listEnchere = surCoinche :: listEnchere
      Some(enchereSurCoinche(current.get))
    } else None
  }

  // Receiving a PlayerTypeChangeException means a human player was replaced by a bot
  // We stop waiting for input and then asks the bot for a bid
  def getEnchere: Option[Enchere] = {
    Partie.Printer.tourJoueurEnchere(Partie.currentPlayer)
    def aux:Option[Enchere] = try{
      Partie.currentPlayer match {
        case b:BotTrait => b.effectuerEnchere(listEnchere)
        case j:Joueur => effectuerEnchere()
      }
    } catch {case `PlayerTypeChangeException` => aux}
    aux
  }

  /**
   *
   * @return Option sur enchere : (couleur,contrat,id,coinche)
   *         id : 0 pour sud, 1 pour ouest, 2 pour nord, 3 pour est
   *         coinche : 1 = pas de coinche, 2 = coinché, 4 = contré
   */
  def enchere():Option[Enchere] = {


    // On reinitialise les variables globales
    var nbPasse = 0
    listEnchere = List()

    Partie.state = Partie.State.bidding


    // Boucle principale lors des encheres
    while ( (nbPasse < 3)                      // apres 3 passes on finit les encheres
      || (current == None && nbPasse == 3)){   // sauf s'il n'y a pas eu d'annonce,auquel cas on attend le dernier joueur
      if (current.exists(_.coinche > 1)) {getSurCoinche; nbPasse = 4}
      else {
        val enchere = getEnchere
        if (enchere.isEmpty) nbPasse=nbPasse+1
        else {
          //une enchere a etait faite, on remet le nombre de passe a zero
          nbPasse=0
          listEnchere = enchere.get :: listEnchere
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
