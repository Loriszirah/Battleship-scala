package utils

import models.{HardAI, LowAI, MediumAI, Player}

import scala.io.StdIn.readLine
import scala.util.Random

object Input {

  /**
    * Ask the user to enter a username with the text given in parameter
    * @param text the string to print for asking the username
    * @return the string enter by the user
    */
  def getUsername(text: String): String = {
    readLine(text)
  }

  /**
    * Ask the user to enter an x and an y
    * @return a tuple (Char, Int) corresponding to the input of the user
    */
  def getCoordinates: (Char, Int) = {
    val x = readLine(s"Enter the x coordinate (Between ${Board.startX} and ${Board.endX}) : ")
    x.toCharArray.length  match {
      case 1 =>
        val charX = x.charAt(0)
        if (!Board.xInBoard(charX)) {
          println(s"x needs to be between ${Board.startX} and ${Board.endX}")
          getCoordinates
        } else {
          val y = readLine(s"Enter the y coordinate (Between ${Board.startY} and ${Board.endY}) : ")
          y match {
            case number if number.matches("\\d+") =>
              if (!Board.yInBoard(number.toInt)) {
                println(s"y needs to be between ${Board.startY} and ${Board.endY}")
                getCoordinates
              } else{
                (charX, number.toInt)
              }
            case _ => getCoordinates
          }
        }
      case _ => getCoordinates
    }
  }

  def getCoordinatesShip: (Char, Int, String) = {
    val x = readLine(s"Enter the x coordinate (Between ${Board.startX} and ${Board.endX}) : ")
    x.toCharArray.length match {
      case 1 =>
        val charX = x.charAt(0)
        if(!Board.xInBoard(charX)){
          println(s"x needs to be between ${Board.startX} and ${Board.endX}")
          getCoordinatesShip
        }
        val y = readLine(s"Enter the y coordinate (Between ${Board.startY} and ${Board.endY}) : ")
        y match {
          case number if number.matches("\\d+") =>
            if (!Board.yInBoard(number.toInt)) {
              println(s"y needs to be between ${Board.startY} and ${Board.endY}")
              getCoordinatesShip
            }
            val orientation = readLine("Choose the orientation : H : Horizontal || V : Vertical ")
            orientation match {
              case "H" | "V" =>
                (charX, y.toInt, orientation)
              case _ =>
                println("You have to choose the orientation H or V")
                getCoordinatesShip
            }
          case _ =>
            println("y needs to be between '1' and '10'")
            getCoordinatesShip
        }
      case _ =>
        println("x needs to be composed of only one character")
        getCoordinatesShip
    }
  }


  /**
    * Ask the user what he wants to do next. This function is generally called after the end of a game
    * @return a string representing the choice of the user.
    *         - "1" for a Rematch
    *         - "2" for staring a new game
    *         - "3" for quiting the game
    */
  def getEndGameOption: String = {
    val option: String = readLine(
      "Do you want to: \n" +
        "1: Rematch \n" +
        "2: Start a new game \n" +
        "3: Quit the game \n")
    option match {
      case "1" | "2" | "3" => option
      case _ => getEndGameOption
    }
  }

  /**
    * Ask the user which mode he wants to play.
    * @return a string representing the choice of the user :
    *         - "1" for AI vs AI
    *         - "2" for Human vs AI
    *         - "3" for Human vs Human
    *         - "4" for running the test on the AIs
    */
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

  /**
    * Ask the user the level of an AI
    * @param username the username of the AI
    * @param random the random object of the AI
    * @return the AI choose by the user
    */
  def getAILevel(username: String, random: Random): Player = {
    val AILevel: String = readLine("There is 3 levels of IA. Which one do you want to play against ? L: Low, M: Medium, H: Hard")
    AILevel match {
      case "L" =>
        LowAI(username = "LowAI " + username, random = random)
      case "M" =>
        MediumAI(username = "MediumAI " + username, random = random)
      case "H" =>
        HardAI(username = "HardAI " + username, random = random)
      case _ =>
        println("Unspecified AILevel")
        getAILevel(username, random)
    }
  }
}
