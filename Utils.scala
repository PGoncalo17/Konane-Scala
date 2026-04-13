package Project

import Project.MyRandom 
import scala.io.StdIn.readLine

object Utils {
    //show initial prompt
    def showInitialPrompt(): Unit = { //Initial Prompt
        println("\n(d)imension, (s)tart game or (q)uit.")
    }

    //show prompt during game
    def showPromptInGame(): Unit = {
        println("\n(r)estart the game or q(uit).")
    }

    //get user's input
    def getUserInput(): String = readLine.trim.toUpperCase
    
    //build a single line   
    def buildLine(line:Int, col:Int, maxCols:Int, board:Board): String = {
        (col >= maxCols) match {
            case true => ""  //end of line
            case false =>        
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
        (line >= maxLines) match {
            case true => ""     //end of colunm
            case false =>
                val lineSymbols = buildLine(line, 0, maxCols, board)        //build line
                val lineLable = if(line < 10) " " + line.toString + " " else line.toString + " "    //fix spaces for numbers < 10
                lineLable + lineSymbols + "\n" + buildAllLines(line + 1, maxLines, maxCols, board)  //next line
        }
    }

    //print board
    def printBoard(board: Board, lines: Int, cols: Int): Unit = {
        //aux recursive function to create a list of numbers for each colunm
        def buildRange(n: Int): List[Int] = {
            if(n >= cols) Nil else n::buildRange(n + 1)
        }
        val colIndexes = buildRange(0)      //Create list of numbers for each colunm
        val headderLetters = (colIndexes foldRight ("")){ (c, acc) =>   //foldRight to put numbers in right place
            val letter = (c < 10) match {
                case true => s" $c " 
                case false => s"$c "
            }
            letter + acc
            }
        println("   " + headderLetters)
        println(buildAllLines(0, lines, cols, board))       //build all board
    }

    //print game over
    def printGameOver(): Unit = println("\n=== GAME OVER ===")

    //print program over
    def printProgramOver(): Unit = println("\n== PROGRAM OVER===")
    
}