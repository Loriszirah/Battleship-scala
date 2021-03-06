package models

trait Player{
    val username: String
    val fleet: Fleet
    val listShotsGiven: Set[Square]
    val listShotsReceived: Set[Square]
    val listSinkShips: Set[Ship]

  /**
    * Create a fleet containing all the ships for each typeChip inside the ShipsArray array and return a new Player
    * with the updated fleet
    * @param listShips An array of type of ships that need to be placed
    * @return A new player of class Player updated with the new fleet
    */
  def placeShips(listShips: List[TypeShip]): Player

  /**
    * Get the square where the player wants to shoots
    * @return a models.Square representing the position where the player wants to shoot
    */
  def shoot(): Square

  /**
    * Create a ship of type typeShip
    * @param typeShip the type of the ship to place
    * @return a new ship placed on the board
    */
  def createShip(typeShip: TypeShip): Ship

  /**
    * Handle the behavior when the result of a shot is received
    * @param square the square where this player has shot
    * @param hasTouched a boolean to know if a ship of the other player has been touched by the shot on this square
    *                   True if the shot at this square has touched a ship
    *                   False otherwise
    * @param sinkShip a ship option containing the ship destroyed by the shot
    * @return the new player updated
    */
  def hasShot(square: Square, hasTouched: Boolean, sinkShip: Option[Ship]): Player

  /**
    * Handle the behavior when a player is receiving a shot
    * @param square the square where the other player has chose to shoot
    * @return the new player updated, a boolean True if a ship has been touched by this shot, False otherwise and a
    *         Ship option with the ship that has been potentially down by this shot
    */
  def receivedShot(square: Square): (Player, Boolean, Option[Ship])

  /**
    * Check if a player lose the game e.g all the ships are sunk
    * @return True of all the ships are sunk, False otherwise
    */
  def didLose(): Boolean = fleet.listShips.dropWhile(ship => ship.positions.dropWhile(square => square.state == State.SINK).isEmpty).isEmpty

  /**
    * Reset the attributes of the player
    * @return a new player with initialisation attributes
    */
  def reset(): Player
}
