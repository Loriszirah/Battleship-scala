package main

import models._
import utils.Board

import scala.io.StdIn.readLine
import scala.annotation.tailrec
import scala.util.Random

object Battleship extends App {

  val randomP1: Random = new Random(3)
  val randomP2: Random = new Random(2)
  mainLoop(GameState(null, null), randomP1, randomP2)

  def mainLoop(gameState: GameState, randomP1: Random, randomP2: Random): Unit ={
    println("Welcome to the Battleship Game...")
    println(s""" ${Console.BLUE}

                             _           _   _   _           _     _
                            | |         | | | | | |         | |   (_)
                            | |__   __ _| |_| |_| | ___  ___| |__  _ _ __
                            | '_ \\ / _` | __| __| |/ _ \\/ __| '_ \\| | '_ \\
                            | |_) | (_| | |_| |_| |  __/\\__ \\ | | | | |_) |
                            |_.__/ \\__,_|\\__|\\__|_|\\___||___/_| |_|_| .__/
                                                                    | |
                                                                    |_|
       ${Console.RESET}""")
    println(
      s""" ${Console.GREEN}

                                                |\\/|
                                                ---
                                               / | [
                                        !      | |||
                                      _/|     _/|-++'
                  ${Console.RED}.!,${Console.GREEN}      _      +  +--|    |--|--|_ |-
                  ${Console.RED}-${Console.YELLOW}*${Console.RED}-${Console.GREEN}=====| |{ /|__|   |/\\__|  |--- |||__/
                  ${Console.RED}'|`${Console.GREEN}         +---------------___[}-_===_.'____                 /\\
                          ____`-' ||___-{]_| _[}-  |     |_[___\\==--            \\/   _
           __..._____--==/___]_|__|_____________________________[___\\==--____,------' .7
          |                                                               IG5-CARRIER/
           \\________________________________________________________________________|
       ${Console.RESET}""")
    var player1: Player = null
    var player2: Player = null
    val gameMode: String = getGameMode
    gameMode match {
      case "HvsAI" =>
        val usernameP1: String = readLine("Enter your username: ")
        player1 = Human(usernameP1)
        player2 = getAI(randomP2)
      case "HvsH" =>
        val usernameP1: String = readLine("Enter the username of the player 1: ")
        player1 = Human(usernameP1, Fleet())
        val usernameP2: String = readLine("Enter the username of the player 2: ")
        player2 = Human(usernameP2)
      case "AIvsAI" =>
        println("First AI")
        player1 = getAI(randomP1)
        println()
        println("Second AI")
        player2 = getAI(randomP2)
      case _ =>
        println("Unknown game mode")
        mainLoop(gameState, randomP1, randomP2)
    }

    // val typeShipsToPlace: List[TypeShip] = List(Ship.CARRIER, Ship.BATTLESHIP, Ship.CRUISER, Ship.DESTROYER, Ship.SUBMARINE)
    val newPlayer1: Player = player1.placeShips(Board.typeShipsToPlace)
    val newPlayer2: Player = player2.placeShips(Board.typeShipsToPlace)
    gameLoop(GameState(newPlayer1, newPlayer2))
  }

  @tailrec
  def gameLoop(gameState: GameState): Unit ={
    println(s"Turn of ${gameState.player1.username}...")
    Board.renderGridReceived(gameState.player1)
    Board.renderGridGiven(gameState.player1)
    val square: Square= gameState.player1.shoot()
    val (newPlayer2, touched, sinkShip) = gameState.player2.receivedShot(square)
    val newPlayer1 = gameState.player1.hasShot(square, touched, sinkShip)
    Board.renderGridReceived(newPlayer1)
    Board.renderGridGiven(newPlayer1)

    if(newPlayer1.didLose()){
      println(s"${newPlayer2.username} won the Game. Well Played!!!!")
    } else if(newPlayer2.didLose()){
      println(s"${newPlayer1.username} won the Game. Well Played!!!!")
    } else{
      gameLoop(GameState(newPlayer2, newPlayer1))
    }
  }

  @tailrec
  def getGameMode: String = {
    val gameMode: String = scala.io.StdIn.readLine("Do you want to play => 1: AI vs AI // 2: Human vs AI // 3: Human vs Human")
    gameMode match {
      case "1" => "AIvsAI"
      case "2" => "HvsAI"
      case "3" => "HvsH"
      case _ =>
        println("Please enter a number between 1 and 3")
        getGameMode
    }
  }

  @tailrec
  def getAI(random: Random): Player = {
    val AIlevel: String = readLine("There is 3 levels of IA. Which one do you want to play against ? L: Low, M: Medium, H: Hard")
    AIlevel match {
      case "L" =>
        LowAI(random = randomP2)
      case "M" =>
        MediumAI(random = randomP2)
      case "H" =>
        HardAI(random = randomP2)
      case _ =>
        println("Unspecified AILevel")
        getAI(random)
    }
  }
}
