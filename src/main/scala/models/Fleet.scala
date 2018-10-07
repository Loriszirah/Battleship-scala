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
    * Check if the shot represented by the square in parameter touched the fleet
    * @param square the square representing the position to check
    * @return - the fleet updated with the shot
    *         - a boolean True if the shot touched a ship, False otherwise
    *         - an option with the ship sunk if exists, None otherwise
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

  /**
    * Check if the ship given in parameter is overlapping one of the ship already in the fleet
    * @param ship the ship to check
    * @return a boolean, True if the ship given in parameter is overlapping one of the ship in the fleet, False otherwise
    */
  def isOverlaps(ship: Ship): Boolean = {
    if(listShips.isEmpty) false
    else listShips.dropWhile(shipFleet => !shipFleet.isOverlaps(ship)).nonEmpty
  }

  /**
    * Check if the fleet is sunk e.g if all the ships inside the fleet are sunk
    * @return a boolean, True if the fleet is sunk, False otherwise
    */
  def isSunk: Boolean = {
    this.listShips.dropWhile(ship => ship.isSunk).isEmpty
  }

  /**
    * Check if the fleet is touched by the square given in parameter e.g if one of the ship of the fleet is touched
    * @param square the square representing the position to check
    * @return a boolean, True if one ship is touched by the square, False otherwise
    */
  def isTouched(square: Square): Boolean = {
    listShips.dropWhile(ship => !ship.isTouched(square)).nonEmpty
  }
}
