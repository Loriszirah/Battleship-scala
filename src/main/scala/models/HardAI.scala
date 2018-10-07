package models

import utils.Board

import scala.annotation.tailrec
import scala.util.Random

case class HardAI(username: String = "HardAI", fleet: Fleet = Fleet(), listShotsGiven: Set[Square] = Set(),
                 listShotsReceived: Set[Square] = Set(), listSinkShips: Set[Ship] = Set(), random: Random) extends Player {

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
    def shootTailRec(listShotsGivenLeft: Set[Square]): Square = {
      val hit: Option[Square] = listShotsGivenLeft.find(square => square.state == State.HIT)
      if(hit.isDefined) {
        if (Board.nextX(hit.get.x).isDefined &&
          !listShotsGivenLeft.exists(square => square.x == Board.nextX(hit.get.x).get && square.y == hit.get.y)) { // Right
          Square(Board.nextX(hit.get.x).get, hit.get.y, State.SHOT)
        } else {
          if (Board.nextY(hit.get.y).isDefined &&
            !listShotsGivenLeft.exists(square => square.x == hit.get.x && square.y == Board.nextY(hit.get.y).get)) { // Down
            Square(hit.get.x, Board.nextY(hit.get.y).get, State.SHOT)
          } else {
            if (Board.previousX(hit.get.x).isDefined &&
              !listShotsGivenLeft.exists(square => square.x == Board.previousX(hit.get.x).get && square.y == hit.get.y)) {
              // Left
              Square(Board.previousX(hit.get.x).get, hit.get.y, State.SHOT)
            } else {
              if (Board.previousY(hit.get.y).isDefined &&
                !listShotsGivenLeft.exists(square => square.x == hit.get.x && square.y == Board.previousY(hit.get.y).get)) {
                // Up
                Square(hit.get.x, Board.previousY(hit.get.y).get, State.SHOT)
              } else {
                shootTailRec(listShotsGivenLeft.map(square => if(square.x == hit.get.x && square.y == hit.get.y) square.copy(state = State.SHOT) else square))
              }
            }
          }
        }
      } else {
        val x: Char = (this.random.nextInt(Board.endX.toInt-Board.startX.toInt+1)+Board.startX.toInt).toChar
        val y: Int = this.random.nextInt(Board.endY-Board.startY+1)+Board.startY
        val squareShot: Square = Square(x, y, State.SHOT)
        if(listShotsGivenLeft.exists(square => square.x == squareShot.x && square.y == squareShot.y)) {
          shootTailRec(listShotsGivenLeft)
        } else {
          squareShot
        }
      }
    }
    shootTailRec(listShotsGiven)
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

  override def reset(): Player = HardAI(random = random)
}
