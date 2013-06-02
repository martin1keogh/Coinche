package GameLogic

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 02/06/13
 * Time: 21:15
 * To change this template use File | Settings | File Templates.
 */
class EnchereTest extends FlatSpec{
 "A legal bid" must "be positive" in {
   assert(!Enchere.annonceLegal(-10))
 }
  it must "be a multiple of 10" in {
    assert(!Enchere.annonceLegal(15))
    assert(!Enchere.annonceLegal(24))
  }
  it must "be equal to or lower than 160" in {
    assert(!Enchere.annonceLegal(170))
    assert(Enchere.annonceLegal(160))
  }
  it must "be greater than the last one" in {
    Enchere.current = Some(new Enchere(0,100,0,0))
    assert(Enchere.annonceLegal(110))
    assert(!Enchere.annonceLegal(90))
    Enchere.current = None
  }
  it must "be equal to or greater than 80 if it is the first one" in {
    Enchere.current = None
    assert(Enchere.annonceLegal(80))
    assert(!Enchere.annonceLegal(70))
    assert(Enchere.annonceLegal(110))
  }
  "To still be able to bid, the last bid" must "be lower than 160" in {
    Enchere.current = Some(new Enchere(0,160,0,0))
    assert(Enchere.annonceImpossible())
    Enchere.current = None
  }
  it must "not have been 'coinché'" in {
    Enchere.current = Some(new Enchere(0,130,0,2))
  }
}
