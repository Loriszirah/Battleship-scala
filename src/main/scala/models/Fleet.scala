package models

import scala.collection.immutable.Set

case class Fleet(listShips: Set[Ship] = Set()) {

  /**
    *
    * @param square the square where the player wants to shoot
    * @return
    */
  def hasTouched(square: Square): (Fleet, Boolean, Boolean) = ???
    //this.copy(this.listShips.map(ship => ship.hasTouched(square)))

  def addShip(ship: Ship): Option[Fleet] = {
    if(!isOverlaps(ship)) {
      ship.showPositions()
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
        if (hitShips.head.isSink) Some(hitShips.head)
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

  def isSink: Boolean = {
    this.listShips.dropWhile(ship => ship.isSink).isEmpty
  }

  def isTouched(square: Square): Boolean = {
    listShips.dropWhile(ship => ship.isTouched(square)).isEmpty
  }

  def isShot(square: Square): Boolean = {
    listShips.dropWhile(ship => ship.isTouched(square)).isEmpty
  }
}
