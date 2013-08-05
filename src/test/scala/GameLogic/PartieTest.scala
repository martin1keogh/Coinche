package GameLogic

import org.scalatest.FlatSpec
import UI.Console.{ReaderConsole, PrinterConsole}

class PartieTest extends FlatSpec{
  val pr = new PrinterConsole
  val Partie = new Partie(pr, new ReaderConsole)

  // pointsPourNS takes the number of points North/South made as the second argument
  // pointsEOtoPointsNS takes the number of points West/East made and
  // return the number of points North/South made.
  def pointsEOtoPointsNS(i:Int) = 162-i

  "When North/South won the bidding, the points" should "go to them if the bid is made" in {
    assert(Partie.pointsPourNS(100,101,0))
    assert(Partie.pointsPourNS(100,162,0))
    assert(Partie.pointsPourNS(100,100,2))
    assert(Partie.pointsPourNS(80,133,2))
  }
  it should "go to West/East if the bid is down" in {
    assert(!Partie.pointsPourNS(100,99,0))
    assert(!Partie.pointsPourNS(80,60,2))
    assert(!Partie.pointsPourNS(140,0,0))
  }
  "When West/East won the bidding, the points" should "go to them if the bid is made" in {
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(100),1))
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(101),1))
    assert(!Partie.pointsPourNS(100,pointsEOtoPointsNS(162),3))
    assert(!Partie.pointsPourNS(80,pointsEOtoPointsNS(80),3))
  }
  it should "go to North/South if the bid is down" in {
    assert(Partie.pointsPourNS(100,pointsEOtoPointsNS(99),1))
    assert(Partie.pointsPourNS(80,pointsEOtoPointsNS(60),3))
    assert(Partie.pointsPourNS(140,pointsEOtoPointsNS(0),1))
  }
  "When North/South announce 'capot', the points" should "go to them if it is made" in {
    Partie.capotChute = false
    assert(Partie.pointsPourNS(250,162,0))
    assert(Partie.pointsPourNS(250,162,2))
  }
  it should "go to West/East otherwise" in {
    Partie.capotChute = true
    assert(!Partie.pointsPourNS(250,100,0))
  }
  "When West/East announce 'capot', the points" should "go to them if it is made" in {
    Partie.capotChute = false
    assert(!Partie.pointsPourNS(250,pointsEOtoPointsNS(162),1))
  }
  it should "go to North/South otherwise" in {
    Partie.capotChute = true
    assert(Partie.pointsPourNS(250,pointsEOtoPointsNS(0),1))
  }
  "When North or South announces 'generale', the points" should "go to North/South if it is made" in {
    Partie.generalChute = false
    assert(Partie.pointsPourNS(400,162,0))
    assert(Partie.pointsPourNS(400,162,2))
  }
  it should "go to West/East otherwise" in {
    Partie.generalChute = true
    assert(!Partie.pointsPourNS(400,0,0))
    assert(!Partie.pointsPourNS(400,0,2))
  }
  "When West or East announces 'generale', the points" should "go to West/East if it is made" in {
    Partie.generalChute = false
    assert(!Partie.pointsPourNS(400,pointsEOtoPointsNS(162),1))
    assert(!Partie.pointsPourNS(400,pointsEOtoPointsNS(162),3))
  }
  it should "go to North/South otherwise" in {
    Partie.generalChute = true
    assert(Partie.pointsPourNS(400,pointsEOtoPointsNS(30),1))
    assert(Partie.pointsPourNS(400,pointsEOtoPointsNS(30),3))
  }


}
