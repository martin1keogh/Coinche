/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 19/05/13
 * Time: 21:10
 * To change this template use File | Settings | File Templates.
 */
class Joueur(id:Int) {

  def idPartenaire = (id+2)%4

  override def toString = id match {
    case 0 => "Sud" // le joueur
    case 1 => "Ouest"
    case 2 => "Nord"
    case 3 => "Est"
  }
}
