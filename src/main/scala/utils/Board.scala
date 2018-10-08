package utils

import models._

object Board {
  val typeShipsToPlace: List[TypeShip] = List(Ship.CARRIER, Ship.BATTLESHIP, Ship.CRUISER, Ship.SUBMARINE, Ship.DESTROYER)
  val startX = 'A'
  val endX = 'J'

  val startY = 1
  val endY = 10

  /**
    * Return the next Y on the board
    * @param y an int
    * @return the next Y if it is on the board, None otherwise
    */
  def nextY(y: Int): Option[Int] = {
    if(yInBoard(y)) {
      if (y < endY) {
        Some(y + 1)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
    * Return the previous X on the board
    * @param y an int
    * @return the previous X if its on the board, None otherwise
    */
  def previousY(y: Int): Option[Int] = {
    if(yInBoard(y)) {
      if (y > startY) {
        Some(y - 1)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
    * Return the next X on the board
    * @param x a char
    * @return the next X if it is on the board, None otherwise
    */
  def nextX(x: Char): Option[Char] = {
    if(xInBoard(x)) {
      if (!x.equals(endX)) {
        Some((x + 1).toChar)
      } else {
        None
      }
    }else{
      None
    }
  }

  /**
    * Return the next X on the board
    * @param x a char
    * @return the previous X if it is on board, None otherwise
    */
  def previousX(x: Char): Option[Char] = {
    if(xInBoard(x)) {
      if (!x.equals(startX)) {
        Some((x - 1).toChar)
      } else {
        None
      }
    }else{
      None
    }
  }

  /**
    * Check if the X given in parameter is in the board
    * @param x a char
    * @return a boolean, True if the x given in parameter is in the board, False otherwise
    */
  def xInBoard(x: Char): Boolean = {
    if(x.toInt >= startX.toInt && x.toInt <= endX.toInt) true
    else false
  }

  /**
    * Check if the Y given in parameter is in the board
    * @param y an int
    * @return a boolean, True if the y given in parameter is in the board, False otherwise
    */
  def yInBoard(y: Int): Boolean = {
    if(y >= startY && y <= endY) true
    else false
  }
}
