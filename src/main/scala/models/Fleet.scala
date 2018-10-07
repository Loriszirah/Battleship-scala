package models

import scala.collection.immutable.Set

case class Fleet(listShips: Set[Ship] = Set()) {

  def addShip(ship: Ship): Option[Fleet] = {
    if(!isOverlaps(ship)) {
      Some(this.copy(listShips = this.listShips + ship))
    } else{
      None
    }
  }

  /**
    * Check if the shot
    * @param ship
    * @return
    */
  def receivedShot(square: Square): (Fleet, Boolean, Option[Ship]) = {
    val newListShips: Set[Ship] = listShips.map(ship => ship.receivedShot(square))
    val hitShips: Set[Ship] = newListShips.dropWhile(ship => !ship.isTouched(square))
    val ship: Option[Ship] =
      if(hitShips.nonEmpty) {
        if (hitShips.head.isSunk) Some(hitShips.head)
        else None
      } else {
        None
    }
    val touched: Boolean = hitShips.nonEmpty
    (this.copy(listShips = newListShips), touched, ship)
  }

  def isOverlaps(ship: Ship): Boolean = {
    if(listShips.isEmpty) false
    else listShips.dropWhile(shipFleet => !shipFleet.isOverlaps(ship)).nonEmpty
  }

  def isSunk: Boolean = {
    this.listShips.dropWhile(ship => ship.isSunk).isEmpty
  }

  def isTouched(square: Square): Boolean = {
    listShips.dropWhile(ship => !ship.isTouched(square)).nonEmpty
  }
}
