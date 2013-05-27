package GameLogic

import UI.{Reader,Printer}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 22:31
 * To change this template use File | Settings | File Templates.
 */

case class Enchere(couleur:Int,contrat:Int,id:Int,coinche:Int){

  def couleurToString:String = couleur match {
    case 0 => "Pique"
    case 1 => "Carreau"
    case 2 => "Trefle"
    case 3 => "Coeur"
    case 4 => "Tout Atout"
    case 5 => "Sans Atout"
  }

  def idToString:String = id match {
    case 0 => "Sud"
    case 1 => "Ouest"
    case 2 => "Nord"
    case 3 => "Est"
  }

  def coincheToString:String = coinche match {
    case 1 => "."
    case 2 => ", coinche!"
    case 4 => ", coinche/sur-coinche!!"
  }

  override def toString = contrat+" a "+couleurToString+" par "+idToString+coincheToString
}


object Enchere {

  var listEnchere:List[Enchere] = List()

  //TODO gerer les coinches
  /**
   *
   * @return Option sur enchere : (couleur,contrat,id,coinche)
   *         id : 0 pour sud, 1 pour ouest, 2 pour nord, 3 pour est
   *         coinche : 1 = pas de coinche, 2 = coinché, 4 = contré
   */
  def enchere():Option[Enchere] = {

    var ret:Option[Enchere] = None
    var nbPasse = 0

    def annonceLegal(a:Int):Boolean = {
      val annonceCourante = ret.getOrElse(new Enchere(0,70,0,0)).contrat
      //TODO gerer les capots/generales
      (a%10 == 0 && a > annonceCourante && a < 170)
    }

    def annonceImpossible():Boolean = {
      if (ret.isEmpty) false
      else ret.get.contrat >= 160
    }

    def effectuerEnchere():Option[Enchere] = {
      var ret:Option[Enchere] = None
      val couleur = Reader.getCouleur
      if (!(couleur == 0)) {
        var contrat = -1
        do contrat = Reader.getContrat while (!annonceLegal(contrat))
        ret = Some(new Enchere(couleur-1,contrat,Partie.currentPlayer.id,1))
      }
      if (ret.nonEmpty) listEnchere=ret.get::listEnchere
      ret
    }

    // Boucle principale lors des encheres
    while ( (!annonceImpossible() && (nbPasse < 3) // apres 3 passes on finit les encheres// on arrete les annonces si on ne peut plus monter
            || (ret == None && nbPasse == 3))){   // sauf s'il n'y a pas eu d'annonce,auquel cas on attend le dernier joueur
      Printer.tourJoueurEnchere(Partie.currentPlayer)
      val enchere = effectuerEnchere()
      if (enchere.isEmpty) nbPasse=nbPasse+1
      else {
        //une enchere a etait faite, on remet le nombre de passe a zero
        nbPasse=0
        ret = enchere
      }
      Partie.currentPlayer = Partie.nextPlayer(Partie.currentPlayer)
    }
    listEnchere = List()
    ret
  }
}
