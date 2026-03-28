package Project

import Project.MyRandom 
import scala.io.StdIn.readLine

object Utils {
    //show initial prompt
    def showInitialPrompt(): Unit = { //Initial Prompt
        println("\n(d)imension, (s)tart game or (q)uit.")
    }

    //get user's input
    def getUserInput(): String = readLine.trim.toUpperCase
    
    //build a single line   
    def buildLine(line:Int, col:Int, maxCols:Int, board:Board): String = {
        if(col >= maxCols) ""  //end of line
        else{
            val symbol = board.get((line, col)) match {     //choose symbol for each piece
                case None => "   "
                case Some(Stone.White) => " W "
                case Some(Stone.Black) => " B "
            }
            symbol + buildLine(line, col + 1, maxCols, board)  //next colunm
        }
    }
    //build all matrix
    def buildAllLines(line:Int, maxLines:Int, maxCols:Int, board:Board): String = {
        if(line >= maxLines) ""     //end of colunm
        else {
            val lineSymbols = buildLine(line, 0, maxCols, board)        //build line
            val lineLable = if(line < 10) " " + line.toString + " " else line.toString + " "    //fix spaces for numbers < 10
            lineLable + lineSymbols + "\n" + buildAllLines(line + 1, maxLines, maxCols, board)  //next line
        }
    }

    //print board
    def printBoard(board: Board, lines: Int, cols: Int): Unit = {
        val colIndexes = (0 until cols).toList      //Create list of numbers for each colunm
        val headderLetters = (colIndexes foldRight ("")){ (c, acc) =>   //foldRight to turn numbers in letters and assemble them  in a string
            val letter = ('A' + c).toChar
            " " + letter + " " + acc
            }
        println("   " + headderLetters)
        println(buildAllLines(0, lines, cols, board))       //build all board
    }

    //print game over
    def printGameOver(): Unit = println("\n=== GAME OVER ===")
    
}