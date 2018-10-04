package utils

import models.{Player, Ship, TypeShip}

object Board{
  val typeShipsToPlace: List[TypeShip] = List(Ship.DESTROYER)
  val startX = 'A'
  val endX = 'J'

  val startY = 1
  val endY = 10

  /**
    * Return the next Int on the board
    * @param y
    * @return the next Int if it is on the board, None otherwise
    */
  def nextY(y: Int): Option[Int] ={
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
    * Return the next Char on the board
    * @param x a char
    * @return the next Char if it is on the board, None otherwise
    */
  def nextX(x: Char): Option[Char] ={
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

  def xInBoard(x: Char): Boolean ={
    if(x.toInt >= startX.toInt && x.toInt <= endX.toInt) true
    else false
  }

  def yInBoard(y: Int): Boolean ={
    if(y >= startY && y <= endY) true
    else false
  }
}
