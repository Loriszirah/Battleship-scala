import models.{Ship, Square, State}
import org.scalatest._

class ShipTest extends FlatSpec with Matchers {
  // ############ Constructor test ############
  // ************ Grid test ************
  "The ship" should "be a None if x in not in the grid" in {
    assert(Ship('K', 1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  it should "be a None if y in not in the grid" in {
    assert(Ship('A', -1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  it should "be a None if y and x are not in the grid" in {
    assert(Ship('K', -1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  // ************ Orientation test ************
  it should "be a None if the orientation is not defined" in {
    assert(Ship('A', 1, "undefined", Ship.DESTROYER).isEmpty)
  }

  // ************ Out of bounds test ************
  it should "be a None if the ship doesn't fit in the grid with the vertical orientation" in {
    assert(Ship('A', 9, Ship.VERTICAL, Ship.DESTROYER).isEmpty)
  }

  it should "be a None if the ship doesn't fit in the grid with the horizontal orientation" in {
    assert(Ship('J', 1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  // ************ TypeShip test ************
  it should "keep the same typeShip than the one given in the constructor" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.typeShip == Ship.DESTROYER)
  }

  // ############ Test overlaps method ############
  it should "be overlapsed" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isOverlapping(Ship('B', 1, Ship.HORIZONTAL, Ship.DESTROYER).get))
  }

  it should "not be overlapsed" in {
    assert(!Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isOverlapping(Ship('D', 1, Ship.HORIZONTAL, Ship.DESTROYER).get))
  }

  // ############ Test isTouched method ############
  it should "be hit" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isTouched(Square('A', 1, State.SHOT)))
  }

  it should "not be hit" in {
    assert(!Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isTouched(Square('D', 1, State.SHOT)))
  }

  // ############ Test receivedShot method ############
  it should "modify the cell state to hit" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    assert(shipHit1.positions.head.state == State.HIT)
  }

  it should "not modify one of its cell's state" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('B', 1, State.SHOT))
    assert(shipHit1.positions.head.state == State.OCCUPIED)
  }

  it should "change the states of its cells" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    val shipHit2 = shipHit1.receivedShot(Square('B', 1, State.SHOT))
    assert((shipHit2.positions.head.state == State.SINK) && (shipHit2.positions.tail.head.state == State.SINK))
  }

  it should "not be sink with 1 out of 2 hit" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    assert(!shipHit1.isSunk)
  }

  // ############ Test isSink method ############
  it should "not be sunk with 1 hit and 1 shot with only a common x or y" in {
    //assert(!Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isSunk)
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    val shipHit2 = shipHit1.receivedShot(Square('F', 1, State.SHOT))
    assert(!shipHit2.isSunk)
  }

  it should "be sunk" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    val shipHit2 = shipHit1.receivedShot(Square('B', 1, State.SHOT))
    assert(shipHit2.isSunk)
  }

  // ############ Test generatePosition method ############
  "The positions" should "be defined" in {
    assert(Ship.generatePosition('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).isDefined)
  }

  it should "generate right positions" in {
    assert(Ship.generatePosition('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get ==
      Set(Square('A', 1, State.OCCUPIED), Square('B', 1, State.OCCUPIED)))
  }

  it should "have created 2 squares" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.positions.size == 2)
  }
}
