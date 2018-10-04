package models

import utils.Board

import scala.annotation.tailrec

case class Ship(positions: Set[Square], typeShip: TypeShip) {

  def showPositions(): Unit ={
    println(this.positions.mkString("\n"))
  }

  def receivedShot(square: Square): Ship = {
    if(this.positions.dropWhile(squareShip => (squareShip.x == square.x) && (squareShip.y == square.y)).nonEmpty){
      val newPositions: Set[Square] = this.positions.map(squareShip =>
        if((squareShip.x == square.x) && (squareShip.y == square.y)) Square(square.x, square.y, State.HIT)
        else squareShip)
      if(newPositions.dropWhile(square => square.state == State.HIT).isEmpty){
        this.copy(positions = newPositions.map(square => square.copy(state = State.SINK)))
      } else{
        this.copy(positions = newPositions)
      }
    } else{
      this
    }
  }

  def isOverlaps(ship: Ship): Boolean = {
    positions.
      dropWhile(square =>
        ship.positions.
          dropWhile(newSquare =>
            square.x != newSquare.x || square.y != newSquare.y)
          .isEmpty)
      .nonEmpty
  }

  def isTouched(square: Square): Boolean = {
    positions.dropWhile(squareShip => squareShip.x != square.x || squareShip.y != square.y).nonEmpty
  }

  def isSink: Boolean = {
    this.positions.dropWhile(square => square.state==State.SINK).isEmpty
  }

  override def toString: String = "This ship is type of " + this.typeShip.name
}

case class TypeShip(name: String, size: Int)

object Ship {
  val VERTICAL = "V"
  val HORIZONTAL = "H"
  val CARRIER: TypeShip = TypeShip("Carrier", 5)
  val BATTLESHIP: TypeShip = TypeShip("Battleship", 4)
  val CRUISER: TypeShip = TypeShip("Cruiser", 3)
  val SUBMARINE: TypeShip = TypeShip("Submarine", 3)
  val DESTROYER: TypeShip = TypeShip("Destroyer", 2)

  def apply(positions: Set[Square], typeShip: TypeShip) = new Ship(positions, typeShip)

  def apply(x: Char, y: Int, orientation: String, typeShip: TypeShip): Option[Ship] = {
    val positions = generatePosition(x, y, orientation, typeShip)
    positions match {
      case Some(_) =>
        Some(Ship(positions.get, typeShip))
      case None =>
        None
    }
  }

  def generatePosition(x: Char, y: Int, orientation: String, typeShip: TypeShip): Option[Set[Square]] ={
    @tailrec
    def generatePositionTailRec(x: Char, y: Int, orientation: String, typeShip: TypeShip, positions: Set[Square]): Option[Set[Square]]={
      if(positions.size == typeShip.size){
        Some(positions)
      }
      else {
        orientation match {
          case Ship.HORIZONTAL =>
            val nextX = Board.nextX(x)
            nextX match {
              case Some(_) =>
                generatePositionTailRec(nextX.get, y, orientation, typeShip, positions + Square(x,y, State.OCCUPIED))
              case None =>
                None
            }
          case Ship.VERTICAL =>
            val nextY = Board.nextY(y)
            nextY match {
              case Some(_) =>
                generatePositionTailRec(x, nextY.get, orientation, typeShip, positions + Square(x,y, State.OCCUPIED))
              case None =>
                None
            }
          case _ => None
        }
      }
    }
    if(Board.xInBoard(x) && Board.yInBoard(y)){
      generatePositionTailRec(x, y, orientation, typeShip, Set())
    } else{
      None
    }
  }
}