package main

import models.{Fleet, Human, Player, Square}

object Battleship extends App {

  mainLoop(GameState(null, null))

  def mainLoop(gameState: GameState): Unit ={
      println("Welcome to the Battleship Game...")
      var player1: Player = null
      var player2: Player = null
      val gameMode: String = getGameMode
      gameMode match {
        case "HvsAI" =>
          val usernameP1: String = readLine("Enter your username: ")
          player1 = Human(usernameP1)
          println("There is 3 levels of IA. Which one do you want to play against ? L: Low, M: Medium, H: Hard")
          val IAlevel: Char = readChar()
        case "HvsH" =>
          val usernameP1: String = readLine("Enter the username of the player 1: ")
          player1 = Human(usernameP1, Fleet(Set()))
          val usernameP2: String = readLine("Enter the username of the player 2: ")
          player2 = Human(usernameP2)
        case "AIvsAI" =>
        case _ =>
          println("Unknown game mode")
          mainLoop(gameState)
      }

      // val typeShipsToPlace: List[TypeShip] = List(Ship.CARRIER, Ship.BATTLESHIP, Ship.CRUISER, Ship.DESTROYER, Ship.SUBMARINE)
      val newPlayer1: Player = player1.placeShips(Board.typeShipsToPlace)
      val newPlayer2: Player = player2.placeShips(Board.typeShipsToPlace)
      gameLoop(GameState(newPlayer1, newPlayer2))
  }

  def gameLoop(gameState: GameState): Unit ={
    println(s"Turn of ${gameState.player1.username}...")
    val square: Square= gameState.player1.shoot()
    val (newPlayer2, touched, sinkShip) = gameState.player2.receivedShot(square)
    val newPlayer1 = gameState.player1.hasShot(square, touched, sinkShip)
    if(newPlayer1.didLose()){
      println(s"${newPlayer2.username} won the Game. Well Played!!!!")
    } else if(newPlayer2.didLose()){
      println(s"${newPlayer1.username} won the Game. Well Played!!!!")
    } else{
      gameLoop(GameState(newPlayer2, newPlayer1))
    }
  }

  def getGameMode: String = {
    val gameMode: String = scala.io.StdIn.readLine("Do you want to play => 1: AI vs AI // 2: models.Human vs AI // 3: models.Human vs models.Human")
    gameMode match {
      case "1" => "AIvsAI"
      case "2" => "HvsAI"
      case "3" => "HvsH"
      case _ =>
        println("Please enter a number between 1 and 3")
        getGameMode
    }
  }
}
