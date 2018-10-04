import models.{Ship, Square, State}
import org.scalatest._

class ShipTest extends FlatSpec with Matchers {
  // ############ Constructor test ############
  // ************ Grid test ************
  "The ship" should "be a None if x in not in the grid" in {
    assert(Ship('K', 1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  "The ship" should "be a None if y in not in the grid" in {
    assert(Ship('A', -1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  // ************ Orientation test ************
  "The ship" should "be a None if the orientation is not defined" in {
    assert(Ship('A', 1, "undefined", Ship.DESTROYER).isEmpty)
  }

  // ************ Out of bounds test ************
  "The ship" should "be a None if the ship doesn't fit in the grid with the vertical orientation" in {
    assert(Ship('A', 9, Ship.VERTICAL, Ship.DESTROYER).isEmpty)
  }

  "The ship" should "be a None if the ship doesn't fit in the grid with the horizontal orientation" in {
    assert(Ship('J', 1, Ship.HORIZONTAL, Ship.DESTROYER).isEmpty)
  }

  // ############ Test generatePosition method ############
  "The positions" should "be defined" in {
    assert(Ship.generatePosition('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).isDefined)
  }

  it should "generate right positions" in {
    assert(Ship.generatePosition('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get ==
      Set(Square('A', 1, State.OCCUPIED), Square('B', 1, State.OCCUPIED)))
  }

  it should "create the right ship" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get ==
      Ship(Set(Square('A', 1, State.OCCUPIED), Square('B', 1, State.OCCUPIED)), Ship.DESTROYER))
  }

  // ############ Test overlaps method ############
  "The ship" should "be overlapsed" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isOverlaps(Ship('B', 1, Ship.HORIZONTAL, Ship.DESTROYER).get))
  }

  "The ship" should "not be overlapsed" in {
    assert(!Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isOverlaps(Ship('D', 1, Ship.HORIZONTAL, Ship.DESTROYER).get))
  }

  // ############ Test isTouched method ############
  "The ship" should "be hit" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isTouched(Square('A', 1, State.SHOT)))
  }

  "The ship" should "not be hit" in {
    assert(Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isTouched(Square('A', 1, State.SHOT)))
  }

  // ############ Test receivedShot method ############
  "The ship" should "modify the cell state to hit" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    assert(shipHit1.positions.head.state == State.HIT)
  }

  "The ship" should "not modify one of its cell's state" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('B', 1, State.SHOT))
    assert(shipHit1.positions.head.state == State.OCCUPIED)
  }

  "The ship" should "change the states of its cells" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    val shipHit2 = shipHit1.receivedShot(Square('B', 1, State.SHOT))
    assert((shipHit2.positions.head.state == State.SINK) && (shipHit2.positions.tail.head.state == State.SINK))
  }

  "The ship" should "not be sink with 1 out of 2 hit" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    assert(!shipHit1.isSink)
  }

  // ############ Test isSink method ############
  "The ship" should "not be sink with 0 hit" in {
    assert(!Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get.isSink)
  }

  "The ship" should "be sink" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val shipHit1 = ship.receivedShot(Square('A', 1, State.SHOT))
    val shipHit2 = shipHit1.receivedShot(Square('B', 1, State.SHOT))
    assert(shipHit2.isSink)
  }
}
