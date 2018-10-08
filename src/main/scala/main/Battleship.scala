package main

import models._
import utils.{Board, BoardOutput, Input}

import scala.io.StdIn.readLine
import scala.annotation.tailrec
import scala.util.Random
import java.io.{BufferedWriter, File, FileWriter}

object Battleship extends App {

  val randomP1: Random = new Random()
  val randomP2: Random = new Random()
  mainLoop(GameState(null, null, None), randomP1, randomP2)

  /**
    * Initialization loop of the game. Called at the if the user wants to chose a game mode and the username of the
    * players
    * @param gameState the gameState of the game
    * @param randomP1 the random object for the first AI
    * @param randomP2 the random object for the second AI
    */
  @tailrec
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
    val gameMode: String = Input.getGameMode
    gameMode match {
      case "HvsAI" =>
        val usernameP1: String = Input.getUsername("Enter your username: ")
        val player1 = Human(usernameP1)
        val player2 = Input.getAILevel(username = "2", randomP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "HvsH" =>
        val usernameP1: String = Input.getUsername("Enter the username of the player 1: ")
        val player1 = Human(usernameP1)
        val usernameP2: String = Input.getUsername("Enter the username of the player 2: ")
        val player2 = Human(usernameP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "AIvsAI" =>
        println("First AI")
        val player1 = Input.getAILevel(username = "1", randomP1)
        println()
        println("Second AI")
        val player2 = Input.getAILevel(username = "2", randomP2)
        mainGameLoop(gameState.copy(player1 = player1, player2 = player2))
      case "TestAIs" =>
        runTestAIs(randomP1, randomP2)
      case _ =>
        println("Unknown game mode")
        mainLoop(gameState, randomP1, randomP2)
    }
  }

  /**
    * Loop managing the start and the end of the game.
    * @param gameState the gameState of the game
    */
  @tailrec
  def mainGameLoop(gameState: GameState): Unit = {
    val newGameState = initBoard(gameState)
    val finalGameState = gameLoop(gameState.copy(player1 = newGameState.player1, player2 = newGameState.player2))
    println(s"${finalGameState.winner.get.username} won the Game. Well Played!!!!")
    println(s"Number of shots of the player ${finalGameState.player1.username} ${finalGameState.player1.listShotsGiven.size}")
    println(s"Number of shots of the player ${finalGameState.player2.username} ${finalGameState.player2.listShotsGiven.size}")
    println()
    val game: String = Input.getEndGameOption
    game match {
      case "1" => mainGameLoop(GameState(finalGameState.player2.reset(), finalGameState.player1.reset(), None))
      case "2" => mainLoop(GameState(null, null, None), randomP1, randomP2)
      case _ =>
    }
  }

  /**
    * Init the ships of the 2 players in the gameState
    * @param gameState the gameState of the game
    * @return the new gameState with the 2 players updated with the placement of their ships
    */
  def initBoard(gameState: GameState): GameState = {
    val newPlayer1: Player = gameState.player1.placeShips(Board.typeShipsToPlace)
    val newPlayer2: Player = gameState.player2.placeShips(Board.typeShipsToPlace)
    gameState.copy(player1 = newPlayer1, player2 = newPlayer2)
  }

  /**
    * Run the test of the AIs. 100 games are run for each configuration :
    * - LowAI vs MediumAI
    * - LowAI vs HardAI
    * - MediumAI vs HardAI
    * @param randomP1 the random object for the first player
    * @param randomP2 the random object for the second player
    */
  def runTestAIs(randomP1: Random, randomP2: Random): Unit = {
    @tailrec
    def runTestAIsTailRec(firstAI: Player, secondAI: Player, nbGames: Int, scores: List[List[Int]]): Unit = {
      if(nbGames == 0) { // Either print the results or run the next simulation depending on the type of the current AIs
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
            // Save the results in a csv file
            val file = new File("ai_proof.csv")
            val bw = new BufferedWriter(new FileWriter(file))
            bw.write("AI Name; score; AI Name2; score2\n")
            bw.write(s"AI Level Beginner; ${scores.head.head}; Level Medium; ${scores.head(1)}\n")
            bw.write(s"AI Level Beginner; ${scores(1)(0)}; Level Hard; ${scores(1)(1)}\n")
            bw.write(s"AI Medium; ${scores(2)(0)}; Level Hard; ${scores(2)(1)}\n")
            bw.close()
          case (first: LowAI, second: MediumAI) =>
            runTestAIsTailRec(first, HardAI(random = second.asInstanceOf[MediumAI].random), 100, scores)
          case (first: MediumAI, second: LowAI) =>
            runTestAIsTailRec(second, HardAI(random = first.asInstanceOf[MediumAI].random), 100, scores)
          case (first: LowAI, second: HardAI) =>
            runTestAIsTailRec(MediumAI(random = first.asInstanceOf[LowAI].random), second, 100, scores)
          case (first: HardAI, second: LowAI) =>
            runTestAIsTailRec(MediumAI(random = second.asInstanceOf[LowAI].random), first, 100, scores)
          case _ => println("Unknown combination of AIs")
        }
      } else { // Playing a new game
        val newGameState = initBoard(GameState(firstAI, secondAI, None))
        val finalGameState = gameLoop(newGameState)
        // Recursive call depending on the type of the current AIs
        (finalGameState.player1, finalGameState.player2, finalGameState.winner.get) match {
          case (_: LowAI, _: MediumAI, _: LowAI) | (_: MediumAI, _: LowAI, _: LowAI) =>
            val newScores: List[List[Int]] = scores.updated(0, List(scores.head.head+1,scores.head(1)))
            runTestAIsTailRec(secondAI, firstAI,  nbGames-1, newScores)
          case (_: LowAI, _: MediumAI, _: MediumAI) | (_: MediumAI, _: LowAI, _: MediumAI) =>
            val newScores: List[List[Int]] = scores.updated(0, List(scores.head.head,scores.head(1)+1))
            runTestAIsTailRec(secondAI, firstAI, nbGames-1, newScores)
          case (_: LowAI, _: HardAI, _: LowAI) | (_: HardAI, _: LowAI, _: LowAI) =>
            val newScores: List[List[Int]] = scores.updated(1, List(scores(1).head+1,scores(1)(1)))
            runTestAIsTailRec(secondAI, firstAI, nbGames-1, newScores)
          case (_: LowAI, _: HardAI, _: HardAI) | (_: HardAI, _: LowAI, _: HardAI) =>
            val newScores: List[List[Int]] = scores.updated(1, List(scores(1).head,scores(1)(1)+1))
            runTestAIsTailRec(secondAI, firstAI, nbGames-1, newScores)
          case (_: MediumAI, _: HardAI, _: MediumAI) | (_: HardAI, _: MediumAI, _: MediumAI) =>
            val newScores: List[List[Int]] = scores.updated(2, List(scores(2).head+1,scores(2)(1)))
            runTestAIsTailRec(secondAI, firstAI, nbGames-1, newScores)
          case (_: MediumAI, _: HardAI, _: HardAI) | (_: HardAI, _: MediumAI, _: HardAI) =>
            val newScores: List[List[Int]] = scores.updated(2, List(scores(2).head,scores(2)(1)+1))
            runTestAIsTailRec(secondAI, firstAI, nbGames-1, newScores)
          case _ => println("Unknown combination of AIs")
        }
      }
    }
    runTestAIsTailRec(LowAI(random = randomP1), MediumAI(random = randomP2), 100, List(List(0,0), List(0,0), List(0,0)))
  }

  /**
    * Loop for the steps of a game. Every step the players are swapped to alternate the shot
    * @param gameState the gameState of the current game
    * @return the gameState of the end of the current game, the property winner is updated with the winner (either
    *         player 1 or player 2)
    */
  @tailrec
  def gameLoop(gameState: GameState): GameState ={
    if(gameState.player1.isInstanceOf[Human]) {
      println(s"Turn of ${gameState.player1.username}...")
      BoardOutput.renderGridReceived(gameState.player1)
      BoardOutput.renderGridGiven(gameState.player1)
    }
    val square: Square= gameState.player1.shoot()
    val (newPlayer2, touched, sinkShip) = gameState.player2.receivedShot(square)
    val newPlayer1 = gameState.player1.hasShot(square, touched, sinkShip)
    if(gameState.player1.isInstanceOf[Human]) {
      BoardOutput.renderGridReceived(newPlayer1)
      BoardOutput.renderGridGiven(newPlayer1)
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
}
