package GameLogic

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import GameLogic.Enchere._

class EnchereController(implicit Partie:Partie){

  import scala.concurrent.ExecutionContext.Implicits.global
  import UI.Reader._

  var Printer = Partie.Printer
  var Reader = Partie.Reader

  var listEnchere:List[Enchere] = List()
  var current:Option[Enchere] = None

  val enchereNull = new Enchere(Undef(),70,-1,"",-1)
  def couleur = current.getOrElse(enchereNull).couleur
  def contrat = current.getOrElse(enchereNull).contrat
  def id = current.getOrElse(enchereNull).id
  def coinche = current.getOrElse(enchereNull).coinche

  def annonceLegal(j:Joueur,a:Int):Boolean = {
    val annonceCourante = current.getOrElse(enchereNull).contrat
    a>annonceCourante && ( a == 250 || a == 400 || (a%10 == 0 && a < 170)) && Partie.currentPlayer == j
  }

  def coincheValid(j:Joueur) = contrat > 80 && id % 2 != j.id % 2

  def surCoincheValid(j:Joueur) = coinche == 2 && id % 2 == j.id % 2

  def enchereCoinche(e:Enchere):Enchere = {val ret = e;ret.coinche = 2;ret}

  def enchereSurCoinche(e:Enchere):Enchere = {val ret = e;ret.coinche = 4;ret}

  def effectuerEnchere():Option[Enchere] = {
    def readMessage:Option[Enchere] = Reader.getMessage match {
      case (j,Coinche()) if coincheValid(j) => Some(enchereCoinche(current.get))
      case (j,SurCoinche()) if surCoincheValid(j) => Some(enchereSurCoinche(current.get))
      case (j,Passe()) if (j == Partie.currentPlayer) => None
      case (j,Bid(couleur,valeur)) if annonceLegal(j,valeur) => Some(new Enchere(couleur,valeur,j.id,j.nom))
      case _ => readMessage
    }
    val ret = readMessage
    if (ret.nonEmpty) listEnchere=ret.get::listEnchere
    ret
  }

  def getSurCoinche():Option[Enchere] = {
    if (Reader.getSurCoinche.exists(surCoincheValid)) {
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
        Printer.printCoinche()
        nbPasse = 4
        current = getSurCoinche()
      } else {
        Printer.tourJoueurEnchere(Partie.currentPlayer)
        val enchere = effectuerEnchere()
        if (enchere.isEmpty) nbPasse=nbPasse+1
        else {
          //une enchere a etait faite, on remet le nombre de passe a zero
          nbPasse=0
          current = enchere
        }
        Partie.currentPlayer = Partie.nextPlayer(Partie.currentPlayer)
      }
    }
    Partie.state = Partie.State.running

    current
  }
}
