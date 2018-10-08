package models

import utils.{BoardOutput, Input}

import scala.annotation.tailrec

case class Human(username: String = "unknown", fleet: Fleet = Fleet(), listShotsGiven: Set[Square] = Set(),
                 listShotsReceived: Set[Square] = Set(), listSinkShips: Set[Ship] = Set()) extends Player {

  override def placeShips(listShips: List[TypeShip]): Player = {
    @tailrec
    def placeShipTailRec(listShips: List[TypeShip], fleet: Fleet): Fleet ={
      println()
      if(listShips.isEmpty) {
        BoardOutput.renderGridReceived(this.copy(fleet = fleet))
        fleet
      }
      else{
        val newFleet: Option[Fleet] = fleet.addShip(this.createShip(listShips.head))
        if(newFleet.isDefined) {
          BoardOutput.renderGridReceived(this.copy(fleet = newFleet.get))
          placeShipTailRec(listShips.tail, newFleet.get)
        } else {
          println("The ships you placed is superimposed with an other ship already inside your fleet")
          BoardOutput.renderGridReceived(this.copy(fleet = fleet))
          placeShipTailRec(listShips, fleet)
        }
      }
    }
    println(s"Placement of the ships for the player ${this.username}")
    val newFleet: Fleet = placeShipTailRec(listShips, fleet)
    this.copy(fleet = newFleet)
  }

  override def createShip(typeShip: TypeShip): Ship = {
    println(s"Creation of the ship ${typeShip.name} with a size of ${typeShip.size}")
    val (x, y, orientation) = Input.getCoordinatesShip
    orientation match {
      case "H" =>
        val ship = Ship(x, y.toInt, Ship.HORIZONTAL, typeShip)
        ship.getOrElse(createShip(typeShip))
      case "V" =>
        val ship = Ship(x, y.toInt, Ship.VERTICAL, typeShip)
        ship.getOrElse(createShip(typeShip))
    }
  }

  override final def shoot(): Square = {
    println("It is your turn to shoot. Please provides the square where you want to shoot.")
    val (x, y) = Input.getCoordinates
    Square(x, y, State.SHOT)
  }

  override def hasShot(square: Square, hasTouched: Boolean, sinkShip: Option[Ship]): Player = {
    if(this.listShotsGiven.exists(squarePlayer => squarePlayer.x == square.x && squarePlayer.y == square.y)){
      this
    } else {
      if (hasTouched) {
        if (sinkShip.isEmpty) {
          println("Touched !")
          this.copy(listShotsGiven = listShotsGiven + square.copy(state = State.HIT))
        } else {
          println(s"SINK !!! ${sinkShip.get.typeShip.name} with a size of ${sinkShip.get.typeShip.size}")
          val newListShotsGiven: Set[Square] = listShotsGiven + square
          val newListSinkShips: Set[Ship] = listSinkShips + sinkShip.get
          this.copy(listShotsGiven = newListShotsGiven.map(square => if (sinkShip.get.isTouched(square)) square.copy(state = State.SINK) else square), listSinkShips = newListSinkShips)
        }
      } else {
        println("Missed :(")
        this.copy(listShotsGiven = listShotsGiven + square)
      }
    }
  }

  override def receivedShot(square: Square): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.receivedShot(square)
    if(this.listShotsReceived.exists(squarePlayer => squarePlayer.x == square.x && squarePlayer.y == square.y)){
      (this, touched, ship)
    } else {
      if (touched && ship.isDefined) {
        val newListShotsReceived: Set[Square] = (listShotsReceived + square.copy(state = State.SINK)).map(square => {
          val squareShip: Option[Square] = ship.get.positions.find(squareShip => squareShip.x == square.x && squareShip.y == square.y)
          squareShip.getOrElse(square)
        })
        (this.copy(fleet = newFleet, listShotsReceived = newListShotsReceived), touched, ship)
      } else if (touched && ship.isEmpty) {
        (this.copy(fleet = newFleet, listShotsReceived = listShotsReceived + square.copy(state = State.HIT)), touched, ship)
      } else {
        (this.copy(fleet = newFleet, listShotsReceived = listShotsReceived + square), touched, ship)
      }
    }
  }

  override def reset(): Player = Human(username = this.username)
}
