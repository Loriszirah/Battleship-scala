import models._
import org.scalatest._

class HumanTest extends FlatSpec with Matchers {
//  "The human" should "choose a valid shot" in{
//    val human = Human()
//    val shot = human.shoot()
//    assert(Board.xInBoard(shot.x) && Board.yInBoard(shot.y))
//  }

  "The human" should "change the state of the square touched by a shot" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val fleet = Fleet()
    val human = Human(fleet = fleet.addShip(ship).get)
    val (newHuman, touched, shipTouched) = human.receivedShot(Square('A', 1, State.SHOT))
    assert(touched)
    assert(shipTouched.isEmpty)
    assert(newHuman.fleet.listShips.head.positions.head.state == State.HIT)
    assert(newHuman.fleet.listShips.head.positions.tail.head.state == State.OCCUPIED)
    assert(newHuman.listShotsReceived.size == 1)
    assert(newHuman.listShotsReceived.head == Square('A', 1, State.HIT))
  }

  it should "change the state of all the squares in case of a sink received both in the ship concerned and in the listShotsReceived" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val fleet = Fleet()
    val human = Human(fleet = fleet.addShip(ship).get)
    val (newHuman, touched, shipTouched) = human.receivedShot(Square('A', 1, State.SHOT))
    val (newHuman2, touched2, shipTouched2) = newHuman.receivedShot(Square('B', 1, State.SHOT))
    assert(touched2)
    assert(shipTouched2.isDefined)
    assert(shipTouched2.get.typeShip == Ship.DESTROYER)
    assert(newHuman2.fleet.listShips.head.positions.dropWhile(square => square.state == State.SINK).isEmpty)
    assert(newHuman2.listShotsReceived.dropWhile(square => square.state == State.SINK).isEmpty)
  }

  it should "change the state of the square in case of a hit and add it to the list" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val fleet = Fleet()
    val human = Human(fleet = fleet.addShip(ship).get)
    val humanShot = Human()
    val (newHuman, touched, shipTouched) = human.receivedShot(Square('A', 1, State.SHOT))
    val newHumanShot = humanShot.hasShot(Square('A', 1, State.SHOT), touched, shipTouched)
    assert(newHumanShot.listShotsGiven.head == Square('A', 1, State.HIT))
    assert(newHumanShot.listShotsGiven.size == 1)
  }

  it should "change the state of all the squares in the listShotsGiven in case of a sink on the other player" in {
    val ship = Ship('A', 1, Ship.HORIZONTAL, Ship.DESTROYER).get
    val fleet = Fleet()
    val human = Human(fleet = fleet.addShip(ship).get)
    val humanShot = Human()
    val (newHuman, touched, shipTouched) = human.receivedShot(Square('A', 1, State.SHOT))
    val newHumanShot = humanShot.hasShot(Square('A', 1, State.SHOT), touched, shipTouched)
    val (newHuman2, touched2, shipTouched2) = newHuman.receivedShot(Square('B', 1, State.SHOT))
    val newHumanShot2 = newHumanShot.hasShot(Square('B', 1, State.SHOT), touched2, shipTouched2)
    assert(newHumanShot2.listShotsGiven.head == Square('A', 1, State.SINK))
    assert(newHumanShot2.listShotsGiven.tail.head == Square('B', 1, State.SINK))
  }
}