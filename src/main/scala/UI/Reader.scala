package UI

import GameLogic.{Joueur, Enchere}

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 20/05/13
 * Time: 22:13
 * To change this template use File | Settings | File Templates.
 */
object Reader {

  def getCouleur:Int = {
    var couleur = -1
    do {
      println("Quelle couleur (0 pour passer) ?")
      println("1/Pique;2/Carreau;3/Trefle;4/Coeur;5/Tout atout;6/Sans Atout")
      couleur = try {
        readInt()
      } catch {
        case e:NumberFormatException => -1
      }
    } while (couleur < 0 || couleur > 6)
    couleur
  }

  def getContrat:Int = {
    var contrat = -1
    println("Quelle annonce ?")
    contrat = try {
      readInt()
    } catch {
      case e:NumberFormatException => -1
    }
    contrat
  }

  def tourJoueurEnchere(j:Joueur) {
    println(">>>> A "+j+" de parler")
  }

  def pasDePrise() {
    println("Pas de Prise !")
  }

  def enchereFinie(e:Enchere) {
    println("--------------Fin des encheres ----------------")
    println(e)
    println("-----------------------------------------------")
  }

}
