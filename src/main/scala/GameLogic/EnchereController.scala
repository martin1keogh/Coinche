package GameLogic

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

class EnchereController(implicit Partie:Partie){

  import scala.concurrent.ExecutionContext.Implicits.global

  var Printer = Partie.Printer
  var Reader = Partie.Reader

  var listEnchere:List[Enchere] = List()
  var current:Option[Enchere] = None

  val enchereNull = new Enchere(-1,70,-1,"",-1)
  def couleur = current.getOrElse(enchereNull).couleur
  def contrat = current.getOrElse(enchereNull).contrat
  def id = current.getOrElse(enchereNull).id
  def coinche = current.getOrElse(enchereNull).coinche

  def annonceLegal(a:Int):Boolean = {
    val annonceCourante = current.getOrElse(enchereNull).contrat
    a>annonceCourante && ( a == 250 || a == 400 || (a%10 == 0 && a < 170))
  }

  def effectuerEnchere():Option[Enchere] = {
    var ret:Option[Enchere] = None
    val couleur = Reader.getCouleur
    // coinche
    if (couleur == 7) {
      if (listEnchere.exists(_.contrat > 80)) listEnchere.head.coinche = 2
      else return effectuerEnchere()
    }
    else if (couleur > 0 && couleur < 7) {
      var contrat = -1
      do contrat = Reader.getContrat while (!annonceLegal(contrat))
      ret = Some(new Enchere(couleur-1,contrat,Partie.currentPlayer.id,Partie.currentPlayer.nom,1))
    }
    if (ret.nonEmpty) listEnchere=ret.get::listEnchere
    ret
  }

  def getSurCoinche():Unit = {
    def aux() : Unit = {
      if (Reader.getCouleur == 8) listEnchere.head.coinche = 4
      else aux()
    }
    try {Await.result(Future{aux()},5.seconds)}
    catch {case e : Throwable => ()}
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
    while ( (nbPasse < 3) // apres 3 passes on finit les encheres// on arrete les annonces si on ne peut plus monter
      || (current == None && nbPasse == 3)){   // sauf s'il n'y a pas eu d'annonce,auquel cas on attend le dernier joueur
      if (Partie.checkStop()) throw Partie.Stopped()
      if (current.exists(_.coinche > 1)) {
        Printer.printCoinche()
        getSurCoinche()
        return current
      } else
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
    // si c'est le dernier joueur qui a coinché
    // on laisse le temps au autre joueur de surcoinche
    // (ce qui ce fait normalement dans la boucle while)
    if (current.exists(_.coinche == 2)) {
      Printer.printCoinche()
      getSurCoinche()
      return current
    }

    Partie.state = Partie.State.running

    current
  }
}
