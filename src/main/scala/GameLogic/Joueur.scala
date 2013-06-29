package GameLogic

case class Joueur(id:Int, var nom:String) {

  val idPartenaire = (id+2)%4

  val Equipe = {
    if (id%2==0) 'NS
    else 'EO
  }

  var main = List[Card]()

  def rename(n:String):Unit = nom = n

  override def toString = nom
}
