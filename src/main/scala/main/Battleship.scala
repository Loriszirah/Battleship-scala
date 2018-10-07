package main

import models._
import utils.Board

import scala.io.StdIn.readLine
import scala.annotation.tailrec
import scala.util.Random

object Battleship extends App {

  val randomP1: Random = new Random()
  val randomP2: Random = new Random()
  mainLoop(GameState(null, null, None), randomP1, randomP2)

  def mainLoop(gameState: GameState, randomP1: Random, randomP2: Random): Unit ={
    // print("\033[H\033[2J")
    print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
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
    val gameMode: String = getGameMode
    gameMode match {
      case "HvsAI" =>
        val usernameP1: String = readLine("Enter your username: ")
        val player1 = Human(usernameP1)
        val player2 = getAI(username = "2", randomP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "HvsH" =>
        val usernameP1: String = readLine("Enter the username of the player 1: ")
        val player1 = Human(usernameP1)
        val usernameP2: String = readLine("Enter the username of the player 2: ")
        val player2 = Human(usernameP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "AIvsAI" =>
        println("First AI")
        val player1 = getAI(username = "1", randomP1)
        println()
        println("Second AI")
        val player2 = getAI(username = "2", randomP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "TestAIs" =>
        runTestAIs(randomP1, randomP2)
      case _ =>
        println("Unknown game mode")
        mainLoop(gameState, randomP1, randomP2)
    }
  }

  @tailrec
  def mainGameLoop(gameState: GameState): Unit = {
    val newGameState = initBoard(gameState)
    val finalGameState = gameLoop(gameState.copy(player1 = newGameState.player1, player2 = newGameState.player2))
    println(s"${finalGameState.winner.get.username} won the Game. Well Played!!!!")
    println(s"Number of shots of the player ${finalGameState.player1.username} ${finalGameState.player1.listShotsGiven.size}")
    println(s"Number of shots of the player ${finalGameState.player2.username} ${finalGameState.player2.listShotsGiven.size}")
    println()
    val game: String = getEndGameOption
    game match {
      case "1" => mainGameLoop(GameState(finalGameState.player1.reset(), finalGameState.player2.reset(), None))
      case "2" => mainLoop(GameState(null, null, None), randomP1, randomP2)
      case _ =>
    }
  }

  def initBoard(gameState: GameState): GameState = {
    val newPlayer1: Player = gameState.player1.placeShips(Board.typeShipsToPlace)
    val newPlayer2: Player = gameState.player2.placeShips(Board.typeShipsToPlace)
    gameState.copy(player1 = newPlayer1, player2 = newPlayer2)
  }

  def runTestAIs(randomP1: Random, randomP2: Random): Unit = {
    def runTestAIsTailRec(firstAI: Player, secondAI: Player, nbGames: Int, scores: List[List[Int]]): Unit = {
      if(nbGames == 0) {
        (firstAI, secondAI) match {
          case (_: MediumAI, _: HardAI) | (_: HardAI, _: MediumAI)=>
            println("Results of the AIs vs AIs \n\n" +
              "Low AI vs Medium AI 100 games: \n" +
              s"Low: ${scores.head.head} \n" +
              s"Medium ${scores.head(1)} \n \n" +
              "Low AI vs Hard AI 100 games: \n" +
              s"Low: ${scores(1)(0)} \n" +
              s"Hard ${scores(1)(1)} \n \n" +
              "Medium AI vs Hard AI 100 games: \n" +
              s"Medium: ${scores(2)(0)} \n" +
              s"Hard ${scores(2)(1)} \n \n"
            )
          case (first: LowAI, second: MediumAI) =>
            runTestAIsTailRec(first, HardAI(random = second.asInstanceOf[MediumAI].random), 100, scores)
          case (first: MediumAI, second: LowAI) =>
            runTestAIsTailRec(second, HardAI(random = first.asInstanceOf[MediumAI].random), 100, scores)
          case (first: LowAI, second: HardAI) =>
            runTestAIsTailRec(MediumAI(random = first.asInstanceOf[LowAI].random), second, 100, scores)
          case (first: HardAI, second: LowAI) =>
            runTestAIsTailRec(MediumAI(random = second.asInstanceOf[LowAI].random), first, 100, scores)
        }
      } else {
        val newGameState = initBoard(GameState(firstAI, secondAI, None))
        val finalGameState = gameLoop(newGameState)
        (finalGameState.player1, finalGameState.player2, finalGameState.winner.get) match {
          case (_: LowAI, _: MediumAI, _: LowAI) | (_: MediumAI, _: LowAI, _: LowAI) =>
            val newScores: List[List[Int]] = scores.updated(0, List(scores.head.head+1,scores.head(1)))
            runTestAIsTailRec(firstAI, secondAI,  nbGames-1, newScores)
          case (_: LowAI, _: MediumAI, _: MediumAI) | (_: MediumAI, _: LowAI, _: MediumAI) =>
            val newScores: List[List[Int]] = scores.updated(0, List(scores.head.head,scores.head(1)+1))
            runTestAIsTailRec(firstAI, secondAI, nbGames-1, newScores)
          case (_: LowAI, _: HardAI, _: LowAI) | (_: HardAI, _: LowAI, _: LowAI) =>
            val newScores: List[List[Int]] = scores.updated(1, List(scores(1).head+1,scores(1)(1)))
            runTestAIsTailRec(firstAI, secondAI, nbGames-1, newScores)
          case (_: LowAI, _: HardAI, _: HardAI) | (_: HardAI, _: LowAI, _: HardAI) =>
            val newScores: List[List[Int]] = scores.updated(1, List(scores(1).head,scores(1)(1)+1))
            runTestAIsTailRec(firstAI, secondAI, nbGames-1, newScores)
          case (_: MediumAI, _: HardAI, _: MediumAI) | (_: HardAI, _: MediumAI, _: MediumAI) =>
            val newScores: List[List[Int]] = scores.updated(2, List(scores(2).head+1,scores(2)(1)))
            runTestAIsTailRec(firstAI, secondAI, nbGames-1, newScores)
          case (_: MediumAI, _: HardAI, _: HardAI) | (_: HardAI, _: MediumAI, _: HardAI) =>
            val newScores: List[List[Int]] = scores.updated(2, List(scores(2).head,scores(2)(1)+1))
            runTestAIsTailRec(firstAI, secondAI, nbGames-1, newScores)
          case _ => println("issue")
        }
      }
    }
    runTestAIsTailRec(LowAI(random = randomP1), MediumAI(random = randomP2), 100, List(List(0,0), List(0,0), List(0,0)))
  }

  @tailrec
  def gameLoop(gameState: GameState): GameState ={
    if(gameState.player1.isInstanceOf[Human]) {
      println(s"Turn of ${gameState.player1.username}...")
      Board.renderGridReceived(gameState.player1)
      Board.renderGridGiven(gameState.player1)
    }
    val square: Square= gameState.player1.shoot()
    val (newPlayer2, touched, sinkShip) = gameState.player2.receivedShot(square)
    val newPlayer1 = gameState.player1.hasShot(square, touched, sinkShip)
    if(gameState.player1.isInstanceOf[Human]) {
      Board.renderGridReceived(newPlayer1)
      Board.renderGridGiven(newPlayer1)
    }

    if(newPlayer1.didLose()){
      gameState.copy(winner = Some(newPlayer2))
    } else if(newPlayer2.didLose()){
      gameState.copy(winner = Some(newPlayer1))
    } else{
      if(newPlayer1.isInstanceOf[Human]){
        readLine("Press enter to go to the next round")
        //print("\033[H\033[2J") // Only works on Linux
        print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
      }
      gameLoop(gameState.copy(player1 = newPlayer2, player2 = newPlayer1))
    }
  }

  @tailrec
  def getEndGameOption: String = {
    val option: String = readLine(
      "Do you want to: \n" +
        "1: Rematch" +
        "2: Start a new game" +
        "3: Quit the game")
    option match {
      case "1" | "2" | "3" => option
      case _ => getEndGameOption
    }
  }

  @tailrec
  def getGameMode: String = {
    val gameMode: String = scala.io.StdIn.readLine("Do you want to :\n" +
      "   - 1: play AI vs AI\n" +
      "   - 2: play Human vs AI\n" +
      "   - 3: play Human vs Human\n" +
      "   - 4: Run tests on IAs\n")
    gameMode match {
      case "1" => "AIvsAI"
      case "2" => "HvsAI"
      case "3" => "HvsH"
      case "4" => "TestAIs"
      case _ =>
        println("Please enter a number between 1 and 4")
        getGameMode
    }
  }

  @tailrec
  def getAI(username: String, random: Random): Player = {
    val AIlevel: String = readLine("There is 3 levels of IA. Which one do you want to play against ? L: Low, M: Medium, H: Hard")
    AIlevel match {
      case "L" =>
        LowAI(username = "LowAI " + username, random = randomP2)
      case "M" =>
        MediumAI(username = "MediumAI " + username, random = randomP2)
      case "H" =>
        HardAI(username = "HardAI " + username, random = randomP2)
      case _ =>
        println("Unspecified AILevel")
        getAI(username, random)
    }
  }
}
