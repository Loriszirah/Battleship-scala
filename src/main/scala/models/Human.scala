package models

import utils.Board

import scala.annotation.tailrec

case class Human(username: String = "unknown", fleet: Fleet = Fleet(), listShotsGiven: Set[Square] = Set(),
                 listShotsReceived: Set[Square] = Set(), listSinkShips: Set[Ship] = Set()) extends Player {

  override def placeShips(listShips: List[TypeShip]): Player = {
    @tailrec
    def placeShipTailRec(listShips: List[TypeShip], fleet: Fleet): Fleet ={
      if(listShips.isEmpty) fleet
      else{
        val newFleet: Option[Fleet] = fleet.addShip(this.createShip(listShips.head))
        if(newFleet.isDefined){
          placeShipTailRec(listShips.tail, newFleet.get)
        } else {
          println("The ships you placed is superimposed with an other ship already inside your fleet")
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
    val x = readLine(s"Enter the x coordinate (Between ${Board.startX} and ${Board.endX}) : ")
    x.toCharArray.length match {
      case 1 =>
        val charX = x.charAt(0)
        if(!Board.xInBoard(charX)){
          println(s"x needs to be between ${Board.startX} and ${Board.endX}")
          createShip(typeShip)
        }
        val y = readLine(s"Enter the y coordinate (Between ${Board.startY} and ${Board.endY}) : ")
        y match {
          // TODO : Try to understand
          case number if number.matches("\\d+") =>
            if (!Board.yInBoard(number.toInt)) {
              println(s"y needs to be between ${Board.startY} and ${Board.endY}")
              createShip(typeShip)
            }
            val orientation = readLine("Choose the orientation : H : Horizontal || V : Vertical ")
            orientation match {
              case "H" =>
                val ship = Ship(charX, y.toInt, Ship.HORIZONTAL, typeShip)
                if(ship.isDefined){
                  ship.get
                } else{
                  createShip(typeShip)
                }
                /*ship match {
                  case Some(ship) =>
                    ship
                  case _ =>
                    createShip(typeShip)
                }*/
              case "V" =>
                val ship = Ship(charX, y.toInt, Ship.VERTICAL, typeShip)
                if(ship.isDefined){
                  ship.get
                } else{
                  createShip(typeShip)
                }
//                ship match {
//                  case Some(ship) =>
//                    ship
//                  case _ =>
//                    createShip(typeShip)
//                }
              case _ =>
                println("You have to choose the orientation H or V")
                createShip(typeShip)
            }
          case _ =>
            println("y needs to be between '1' and '10'")
            createShip(typeShip)
        }
      case _ =>
        println("x needs to be composed of only one character")
        createShip(typeShip)
    }
  }

  override def shoot(): Square = {
    println("It is your turn to shoot. Please provides the square where you want to shoot.")
    val x = readLine(s"Enter the x coordinate (Between ${Board.startX} and ${Board.endX}) : ")
    x.toCharArray.length  match {
      case 1 =>
        val charX = x.charAt(0)
        if (!Board.xInBoard(charX)) {
          println(s"x needs to be between ${Board.startX} and ${Board.endX}")
          shoot()
        } else {
          val y = readLine(s"Enter the y coordinate (Between ${Board.startY} and ${Board.endY}) : ")
          y match {
            // TODO : Try to understand
            case number if number.matches("\\d+") =>
              if (!Board.yInBoard(number.toInt)) {
                println(s"y needs to be between ${Board.startY} and ${Board.endY}")
                shoot()
              } else{
                Square(charX, number.toInt, State.SHOT)
              }
          }
        }
    }
  }

  override def hasShot(square: Square, hasTouched: Boolean, sinkShip: Option[Ship]): Player = {
    if(hasTouched){
      if(sinkShip.isEmpty) {
        println("Touched !")
        this.copy(listShotsGiven = listShotsGiven + square.copy(state = State.HIT)).asInstanceOf[Player]
      } else {
        println(s"SINK !!! ${sinkShip.get.typeShip.name} with a size of ${sinkShip.get.typeShip.size}")
        val newListShotsGiven: Set[Square] = listShotsGiven + square
        val newListSinkShips: Set[Ship] = listSinkShips + sinkShip.get
        this.copy(listShotsGiven = newListShotsGiven.map(square => if(sinkShip.get.isTouched(square)) square.copy(state = State.SINK) else square), listSinkShips = newListSinkShips)
      }
    } else{
      println("Missed :(")
      this.copy(listShotsGiven = listShotsGiven + square)
    }
  }

  override def receivedShot(square: Square): (Player, Boolean, Option[Ship]) = {
    val (newFleet: Fleet, touched: Boolean, ship: Option[Ship]) = fleet.receivedShot(square)
    (this.copy(fleet = newFleet, listShotsReceived = listShotsReceived + square), touched, ship)
  }

  override def didLose(): Boolean = {
    fleet.listShips.dropWhile(ship => ship.positions.dropWhile(square => square.state == State.SINK).isEmpty).isEmpty
  }
}