package models

import utils.Board

import scala.annotation.tailrec
import scala.util.Random

case class MediumAI(username: String = "MediumAI", fleet: Fleet = Fleet(), listShotsGiven: Set[Square] = Set(),
                 listShotsReceived: Set[Square] = Set(), listSinkShips: Set[Ship] = Set(), random: Random)
  extends Player {

  override def placeShips(listShips: List[TypeShip]): Player = {
    @tailrec
    def placeShipTailRec(listShips: List[TypeShip], fleet: Fleet): Fleet ={
      if(listShips.isEmpty) {
        fleet
      }
      else{
        val newFleet: Option[Fleet] = fleet.addShip(this.createShip(listShips.head))
        if(newFleet.isDefined) {
          placeShipTailRec(listShips.tail, newFleet.get)
        } else {
          placeShipTailRec(listShips, fleet)
        }
      }
    }
    val newFleet: Fleet = placeShipTailRec(listShips, fleet)
    this.copy(fleet = newFleet)
  }

  override def shoot(): Square = {
    val x: Char = (this.random.nextInt(Board.endX.toInt-Board.startX.toInt+1)+Board.startX.toInt).toChar
    val y: Int = this.random.nextInt(Board.endY-Board.startY+1)+Board.startY
    val squareShot: Square = Square(x, y, State.SHOT)
    if(listShotsGiven.exists(square => square.x == squareShot.x && square.y == squareShot.y)) {
      shoot()
    } else {
      squareShot
    }
  }

  override def createShip(typeShip: TypeShip): Ship = {
    val x: Char = (this.random.nextInt(Board.endX.toInt-Board.startX.toInt+1)+Board.startX.toInt).toChar
    val y: Int = this.random.nextInt(Board.endY-Board.startY+1)+Board.startY
    val ship: Option[Ship] = this.random.nextInt(2) match {
      case 0 => Ship(x, y, Ship.HORIZONTAL, typeShip)
      case 1 => Ship(x, y, Ship.VERTICAL, typeShip)
    }
    ship.getOrElse(createShip(typeShip))
  }

  override def hasShot(square: Square, hasTouched: Boolean, sinkShip: Option[Ship]): Player = {
    if(this.listShotsGiven.exists(squarePlayer => squarePlayer.x == square.x && squarePlayer.y == square.y)){
      this
    } else {
      if (hasTouched) {
        if (sinkShip.isEmpty) {
          this.copy(listShotsGiven = listShotsGiven + square.copy(state = State.HIT))
        } else {
          val newListShotsGiven: Set[Square] = listShotsGiven + square
          val newListSinkShips: Set[Ship] = listSinkShips + sinkShip.get
          this.copy(listShotsGiven = newListShotsGiven.map(square => if (sinkShip.get.isTouched(square)) square.copy(state = State.SINK) else square), listSinkShips = newListSinkShips)
        }
      } else {
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

  override def reset(): Player = MediumAI(username = this.username, random = this.random)
}
