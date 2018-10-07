package utils

import models._

import scala.annotation.tailrec

object Board {
  val typeShipsToPlace: List[TypeShip] = List(Ship.CARRIER, Ship.BATTLESHIP, Ship.CRUISER, Ship.SUBMARINE, Ship.DESTROYER)
  val startX = 'A'
  val endX = 'J'

  val startY = 1
  val endY = 10

  /**
    * Return the next Int on the board
    * @param y
    * @return the next Int if it is on the board, None otherwise
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
    * Return the next Char on the board
    * @param x a char
    * @return the next Char if it is on the board, None otherwise
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

  def xInBoard(x: Char): Boolean = {
    if(x.toInt >= startX.toInt && x.toInt <= endX.toInt) true
    else false
  }

  def yInBoard(y: Int): Boolean = {
    if(y >= startY && y <= endY) true
    else false
  }

  def generateListSquares(): Set[Square] = {
    @tailrec
    def generateListSquareTailRec(x: Char, y: Int, listSquares: Set[Square]): Set[Square] = {
      if(Board.nextX(x).isDefined){
        generateListSquareTailRec(Board.nextX(x).get, y, listSquares + Square(x, y, State.WATER))
      } else{
        if(Board.nextY(y).isDefined){
          generateListSquareTailRec(Board.startX, Board.nextY(y).get, listSquares + Square(x, y, State.WATER))
        } else{
          listSquares + Square(x, y, State.WATER)
        }
      }
    }
    generateListSquareTailRec(startX, startY, Set())
  }

  @tailrec
  def renderGridBorderXTailRec(x: Char): Unit ={
    if(Board.nextX(x).isDefined){
      print(s"|__${x}__|")
      renderGridBorderXTailRec(Board.nextX(x).get)
    } else{
      println(s"|__${x}__|")
    }
  }

  def renderLegend(): Unit = {

  }

  def renderGridGiven(player: Player): Unit = {
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
    print("   ")
    Board.renderGridBorderXTailRec(Board.startX)
    renderGridGivenTailRec(Board.startX, Board.startY, player)
    println()
  }

  def renderGridReceived(player: Player): Unit = {
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
    print("   ")
    renderGridBorderXTailRec(Board.startX)
    renderGridReceivedTailRec(Board.startX, Board.startY, player)
    println()
  }
}
