package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}
import GameLogic.Joueur._

class PartieTest extends FlatSpec{
  val pr = new PrinterConsole
  val Partie = new Partie(pr, new ReaderConsole)

  // pointsPourNS takes the number of points North/South made as the second argument
  // pointsEOtoPointsNS takes the number of points West/East made and
  // return the number of points North/South made.
  def pointsEOtoPointsNS(i:Int) = 162-i

  "When North/South won the bidding, the points" should "go to them if the bid is made" in {
    assert(Partie.pointsPourNS(100,101,Sud))
    assert(Partie.pointsPourNS(100,162,Sud))
    assert(Partie.pointsPourNS(100,100,Nord))
    assert(Partie.pointsPourNS(80,133,Nord))
  }
  it should "go to West/East if the bid is down" in {
    assert(!Partie.pointsPourNS(100,99,Sud))
    assert(!Partie.pointsPourNS(80,60,Nord))
    assert(!Partie.pointsPourNS(140,0,Sud))
  }
  "When West/East won the bidding, the points" should "go to them if the bid is made" in {
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(100),Ouest))
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(101),Ouest))
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(162),Est))
    assert(!Partie.pointsPourNS(80,pointsEOtoPointsNS(80),Est))
  }
  it should "go to North/South if the bid is down" in {
    assert(Partie.pointsPourNS(100,pointsEOtoPointsNS(99),Ouest))
    assert(Partie.pointsPourNS(80,pointsEOtoPointsNS(60),Est))
    assert(Partie.pointsPourNS(140,pointsEOtoPointsNS(0),Ouest))
  }
  "When North/South announce 'capot', the points" should "go to them if it is made" in {
    Partie.capotChute = false
    assert(Partie.pointsPourNS(250,162,Sud))
    assert(Partie.pointsPourNS(250,162,Nord))
  }
  it should "go to West/East otherwise" in {
    Partie.capotChute = true
    assert(!Partie.pointsPourNS(250,100,Sud))
  }
  "When West/East announce 'capot', the points" should "go to them if it is made" in {
    Partie.capotChute = false
    assert(!Partie.pointsPourNS(250,pointsEOtoPointsNS(162),Ouest))
  }
  it should "go to North/South otherwise" in {
    Partie.capotChute = true
    assert(Partie.pointsPourNS(250,pointsEOtoPointsNS(0),Ouest))
  }
  "When North or South announces 'generale', the points" should "go to North/South if it is made" in {
    Partie.generalChute = false
    assert(Partie.pointsPourNS(400,162,Sud))
    assert(Partie.pointsPourNS(400,162,Nord))
  }
  it should "go to West/East otherwise" in {
    Partie.generalChute = true
    assert(!Partie.pointsPourNS(400,0,Sud))
    assert(!Partie.pointsPourNS(400,0,Nord))
  }
  "When West or East announces 'generale', the points" should "go to West/East if it is made" in {
    Partie.generalChute = false
    assert(!Partie.pointsPourNS(400,pointsEOtoPointsNS(162),Ouest))
    assert(!Partie.pointsPourNS(400,pointsEOtoPointsNS(162),Est))
  }
  it should "go to North/South otherwise" in {
    Partie.generalChute = true
    assert(Partie.pointsPourNS(400,pointsEOtoPointsNS(30),Ouest))
    assert(Partie.pointsPourNS(400,pointsEOtoPointsNS(30),Est))
  }


}
