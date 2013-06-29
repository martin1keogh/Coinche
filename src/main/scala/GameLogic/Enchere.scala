package GameLogic

import Main.Main


case class Enchere(couleur:Int,contrat:Int,id:Int,coinche:Int = 1){

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

  var Printer = Main.Printer
  var Reader = Main.Reader

  var listEnchere:List[Enchere] = List()
  var current:Option[Enchere] = None
  var nbPasse = 0

  def annonceLegal(a:Int):Boolean = {
    val annonceCourante = current.getOrElse(new Enchere(0,70,0,1)).contrat
    //TODO gerer les capots/generales
    (a%10 == 0 && a > annonceCourante && a < 170)
  }

  def annonceImpossible():Boolean = {
    if (current.isEmpty) false
    else (current.get.contrat >= 160 || current.get.coinche > 1)
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


  //TODO gerer les coinches
  /**
   *
   * @return Option sur enchere : (couleur,contrat,id,coinche)
   *         id : 0 pour sud, 1 pour ouest, 2 pour nord, 3 pour est
   *         coinche : 1 = pas de coinche, 2 = coinché, 4 = contré
   */
  def enchere():Option[Enchere] = {
    // On reinitialise les variables globales
    current = None
    nbPasse = 0
    listEnchere = List()

    // Boucle principale lors des encheres
    while ( (!annonceImpossible() && (nbPasse < 3) // apres 3 passes on finit les encheres// on arrete les annonces si on ne peut plus monter
            || (current == None && nbPasse == 3))){   // sauf s'il n'y a pas eu d'annonce,auquel cas on attend le dernier joueur
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
    current
  }
}
