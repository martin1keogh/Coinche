package GameLogic.Bot

import GameLogic.{Enchere, Joueur, Card, Partie}
import GameLogic.Enchere.Couleur

class DumBot(partie:Partie,id:Int,nom:String) extends Bot(partie,id,nom){
  /**
   *
   * @param listEnchere Liste des encheres deja annoncées
   * @return None si Passe, Some(Couleur,Contrat) sinon
   */
  def getCouleurEtContrat(listEnchere: List[Enchere]): Option[(Couleur, Int)] = {
    None
  }

  /**
   *
   * @param jouables Liste des cartes jouables par le bot
   * @param autres Liste des cartes non jouables
   * @param pli Liste des cartes sur la table, dans l'ordre inverse (derniere carte jouée en premiere position)
   * @return Carte jouée par le bot
   */
  def getCard(jouables: List[Card], autres: List[Card], pli: List[(Joueur, Card)])
             (implicit couleurDemande: Option[Couleur]): Card = {
    jouables(0)
  }
}
