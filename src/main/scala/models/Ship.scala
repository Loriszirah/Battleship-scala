package models

import utils.Board

import scala.annotation.tailrec

case class Ship(positions: Set[Square], typeShip: TypeShip) {

  /**
    * Check if the shot touched the ship and update it in consequence
    * @param square the square representing the position of the shot
    * @return the ship updated with the shot e.g the state of the square of the ship updated to HIT if the shot
    *         touched the ship and all the square of the ship updated to SINK if the shot touched the last square of the
    *         ship
    *
    */
  def receivedShot(square: Square): Ship = {
    if(this.positions.dropWhile(squareShip => (squareShip.x != square.x) && (squareShip.y != square.y)).nonEmpty){
      val newPositions: Set[Square] = this.positions.map(squareShip =>
        if((squareShip.x == square.x) && (squareShip.y == square.y)) square.copy(state = State.HIT)
        else squareShip)
      if(newPositions.dropWhile(square => square.state == State.HIT).isEmpty) {
        this.copy(positions = newPositions.map(square => square.copy(state = State.SINK)))
      } else {
        this.copy(positions = newPositions)
      }
    } else{
      this
    }
  }

  /**
    * Check if the ship given in parameter is overlapping this ship
    * @param ship the ship to check
    * @return a boolean, True if the ship given in parameter is overlapping this ship, False otherwise
    */
  def isOverlaps(ship: Ship): Boolean = {
    positions.
      dropWhile(square =>
        ship.positions.
          dropWhile(newSquare =>
            square.x != newSquare.x || square.y != newSquare.y)
          .isEmpty)
      .nonEmpty
  }

  /**
    * Check if the square given in parameter is touching this ship
    * @param square the square representing the position to check
    * @return a boolean, True if the square is touching the ship, False otherwise
    */
  def isTouched(square: Square): Boolean = {
    positions.dropWhile(squareShip => squareShip.x != square.x || squareShip.y != square.y).nonEmpty
  }

  /**
    * Check if this ship is sunk e.g if all the square of this ship are hit
    * @return a boolean, True if this ship is sunk, False otherwise
    */
  def isSunk: Boolean = {
    this.positions.dropWhile(square => square.state==State.SINK).isEmpty
  }
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

  /**
    * Constructor of the ship
    * @param x the start of the ship on the X axis
    * @param y the start of the ship on the Y axis
    * @param orientation the orientation of the ship (either Ship.VERTICAL or Ship.HORIZONTAL)
    * @param typeShip the type of the ship to create
    * @return an option of ship: return a Ship if the ship is in the grid, None otherwise
    */
  def apply(x: Char, y: Int, orientation: String, typeShip: TypeShip): Option[Ship] = {
    val positions = generatePosition(x, y, orientation, typeShip)
    positions match {
      case Some(_) =>
        Some(Ship(positions.get, typeShip))
      case None =>
        None
    }
  }

  /**
    * Generate the Set of squares representing the position of the ship
    * @param x the start of the ship on the X axis
    * @param y the start of the ship on the Y axis
    * @param orientation the orientation of the ship (either Ship.VERTICAL or Ship.HORIZONTAL)
    * @param typeShip the type of the ship to create
    * @return an option set of squares representing the position of the ship. It return None if the at least one square
    *         of the ship is not in the grid
    */
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
              case Some(obj) =>
                generatePositionTailRec(obj, y, orientation, typeShip, positions + Square(x,y, State.OCCUPIED))
              case None =>
                None
            }
          case Ship.VERTICAL =>
            val nextY = Board.nextY(y)
            nextY match {
              case Some(obj) =>
                generatePositionTailRec(x, obj, orientation, typeShip, positions + Square(x,y, State.OCCUPIED))
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