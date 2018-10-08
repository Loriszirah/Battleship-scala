package utils

import models.{Player, Square, State}

import scala.annotation.tailrec

object BoardOutput {
  /**
    * Print the legend of the X axis
    * @param x the char to print
    */
  @tailrec
  def renderGridBorderXTailRec(x: Char): Unit ={
    if(Board.nextX(x).isDefined){
      print(s"|__${x}__|")
      renderGridBorderXTailRec(Board.nextX(x).get)
    } else{
      println(s"|__${x}__|")
    }
  }

  /**
    * Print the legend of the grid of the given shots
    */
  def renderLegendGridGiven(): Unit = {
    println("Legend : \n" +
      "- Nothing " + Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET + "   " +
      "- Shot " + Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET + "   " +
      "- Hit " + Console.YELLOW_B + Console.BLACK + "|[_X_]|" + Console.RESET + "   " +
      "- Sink " + Console.GREEN_B + Console.BLACK + s"|[_${35.toChar}_]|" + Console.RESET)
  }

  /**
    * Print the legend for the grid of the received shots
    */
  def renderLegendGridReceived(): Unit = {
    println("Legend : \n" +
      "- Nothing " + Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET + "   " +
      "- Boat " + Console.GREEN_B + "|[___]|" + Console.RESET + "   " +
      "- Shot " + Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET + "   " +
      "- Hit " + Console.RED_B + Console.WHITE + "|[_X_]|" + Console.RESET + "   " +
      "- Sink " + Console.RED_B + Console.WHITE + s"|[_${35.toChar}_]|" + Console.RESET)
  }

  /**
    * Print the grid of the shots given for the player given in parameter
    * @param player the player concerned
    */
  def renderGridGiven(player: Player): Unit = {
    /**
      * Print the right symbol for the given position and player
      * @param x the x of the position
      * @param y the y of the position
      * @param player the player concerned
      * @param end True if a return line needs to be print, False otherwise
      */
    def printState(x: Char, y: Int, player: Player, end: Boolean): Unit = {
      if(Board.startX == x) {
        if(y == Board.endY) print(y + "_")
        else print(y + "__")
      }
      val square: Option[Square] = player.listShotsGiven.find(square => square.x == x && square.y == y)
      if(square.isDefined){
        square.get.state match {
          case State.SINK => {
            if(end) println(Console.GREEN_B + Console.BLACK + s"|[_${35.toChar}_]|" + Console.RESET)
            else print(Console.GREEN_B + Console.BLACK + s"|[_${35.toChar}_]|" + Console.RESET)
          }
          case State.HIT => {
            if(end) println(Console.YELLOW_B + Console.BLACK + "|[_X_]|" + Console.RESET)
            else print(Console.YELLOW_B + Console.BLACK + "|[_X_]|" + Console.RESET)
          }
          case State.SHOT => {
            if(end) println(Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET)
            else print(Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET)
          }
          case _ => {
            if(end) println(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
            else print(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
          }
        }
      } else {
        if(end) println(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
        else print(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
      }
    }

    /**
      * Loop for printing the grid of the given shots for the player
      * @param x the x of the position to print
      * @param y the y of the position to print
      * @param player the player concerned
      */
    @tailrec
    def renderGridGivenTailRec(x: Char, y: Int, player: Player): Unit = {
      if(Board.nextX(x).isDefined){
        printState(x, y, player, false)
        renderGridGivenTailRec(Board.nextX(x).get, y, player)
      } else{
        if(Board.nextY(y).isDefined){
          printState(x, y, player, true)
          renderGridGivenTailRec(Board.startX, Board.nextY(y).get, player)
        } else{
          printState(x, y, player, true)
        }
      }
    }

    println("*********Grid with the shots given*********")
    renderLegendGridGiven()
    print("   ")
    BoardOutput.renderGridBorderXTailRec(Board.startX)
    renderGridGivenTailRec(Board.startX, Board.startY, player)
    println()
  }

  /**
    * Print the grid of the shots received for the player given in parameter
    * @param player the player concerned
    */
  def renderGridReceived(player: Player): Unit = {
    /**
      * Print the right symbol for the given position and player
      * @param x the x of the position
      * @param y the y of the position
      * @param player the player concerned
      * @param end True if a return line needs to be print, False otherwise
      */
    def printState(x: Char, y: Int, player: Player, end: Boolean): Unit = {
      if(Board.startX == x) {
        if(y == Board.endY) print(y + "_")
        else print(y + "__")
      }
      val square: Option[Square] = player.listShotsReceived.find(square => square.x == x && square.y == y)
      if(square.isDefined) {
        square.get.state match {
          case State.SINK =>
            if(end) println(Console.RED_B + Console.WHITE + s"|[_${35.toChar}_]|" + Console.RESET)
            else print(Console.RED_B + Console.WHITE + s"|[_${35.toChar}_]|" + Console.RESET)
          case State.HIT =>
            if(end) println(Console.RED_B + Console.WHITE + "|[_X_]|" + Console.RESET)
            else print(Console.RED_B + Console.WHITE + "|[_X_]|" + Console.RESET)
          case State.SHOT =>
            if(end) println(Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET)
            else print(Console.BLUE_B + Console.BLACK + "|__X__|" + Console.RESET)
          case _ =>
            if(end) println(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
            else print(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
        }
      } else {
        val square: Option[Option[Square]] = player.fleet.listShips.map(ship => ship.positions.find(square =>
          square.x == x && square.y == y)).find(square => square.isDefined)
        if(square.isDefined){
          if(end) println(Console.GREEN_B + "|[___]|" + Console.RESET)
          else print(Console.GREEN_B + "|[___]|" + Console.RESET)
        } else {
          if(end) println(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
          else print(Console.BLUE_B + Console.BLACK + "|_____|" + Console.RESET)
        }
      }
    }

    /**
      * Loop for printing the grid of the received shots for the player
      * @param x the x of the position to print
      * @param y the y of the position to print
      * @param player the player concerned
      */
    @tailrec
    def renderGridReceivedTailRec(x: Char, y: Int, player: Player): Unit ={
      if(Board.nextX(x).isDefined){
        printState(x, y, player, false)
        renderGridReceivedTailRec(Board.nextX(x).get, y, player)
      } else{
        if(Board.nextY(y).isDefined){
          printState(x, y, player, true)
          renderGridReceivedTailRec(Board.startX, Board.nextY(y).get, player)
        } else{
          printState(x, y, player, true)
        }
      }
    }

    println("*********Grid with the shots received*********")
    renderLegendGridReceived()
    print("   ")
    renderGridBorderXTailRec(Board.startX)
    renderGridReceivedTailRec(Board.startX, Board.startY, player)
    println()
  }
}
