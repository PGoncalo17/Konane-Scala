package Project

import Project.Utils.*
import Project.MyRandom
import scala.annotation.tailrec


type Coord2D = (Int, Int)
type Board = Map[Coord2D, Stone]
enum Stone:
    case Black, White

case class GameState(lines: Int, cols: Int, board: Board = Map(), lstOpenCoords: List[Coord2D] = Nil)

object Konane extends App{

    val r = MyRandom(System.currentTimeMillis())

    mainLoop(GameState(0, 0), r)    //runs all the code

    //main loop
    @tailrec
    def mainLoop(gameState: GameState, r: RandomWithState):Unit = {
        showInitialPrompt()
        val userInput = getUserInput()

        //Input for Dimension
        userInput match {
            case "D" | "d" => 
                println("Lines: ")
                val l = scala.io.StdIn.readInt()
                println("Colunms: ")
                val c = scala.io.StdIn.readInt()
                if((l > 0 && c > 0 && (l % 2 != 0 && c % 2 != 0)) || (l <= 0 || c <= 0)){       //Dimension restrictions
                    println("Invalid dimension. Try again!")
                    mainLoop(gameState, r)
                }else{
                    println("Dimension accepted")
                    mainLoop(gameState.copy(lines = l, cols = c), r)
                }
            

            //Input for game
            case "S" | "s" =>
               if(gameState.lines > 0 && gameState.cols > 0){
                    val (board, newR, initialLstOpenCoords) = initBoard(gameState.lines, gameState.cols, r) //Initialize board
                    println("\n--- GAME START --- \n Black's Turn")
                    printBoard(board, gameState.lines, gameState.cols)
                    gameLoop(board, initialLstOpenCoords, Stone.Black, newR, gameState.lines, gameState.cols)   //Black player starts
                    mainLoop(gameState, newR)
                } else{
                    println("Invalid Dimension. Create it in (d)imension first")
                    mainLoop(gameState, r)
                }

            //Input for quit
            case "Q" | "q" =>
                printProgramOver()

            //Invalid input
            case _ =>
                println("Wrong input. Try again")
                mainLoop(gameState, r)
        }
        
    }

    //game loop
    @tailrec
    def gameLoop(board:Board, lstOpenCoords:List[Coord2D], player:Stone, r:RandomWithState, lines:Int, cols:Int): Unit = {
        val (resultBoard, newR, nextLstOpenCoords, coordFrom, coordTo) = playRandomly(board, r, player, lstOpenCoords, randomMove)  //tries to make a move
        resultBoard match{
            case None =>    //case there are no more valid movements
                val winner = if(player == Stone.Black) "White" else "Black"
                println(s"\nNo more moves for ${player}. $winner WINS!")
                printGameOver()
            case Some(newBoard) =>  //case there are more movements
                val pName = if(player == Stone.Black) "Black" else "White"
                val pNextPlayer = if(player == Stone.Black) "White" else "Black"
                println(s"\nPlayer $pName played from ${coordFrom.getOrElse("?")} to ${coordTo.getOrElse("?")}. $pNextPlayer's turn next")
                printBoard(newBoard, lines, cols)

                val nextPlayer = if(player == Stone.Black) Stone.White else Stone.Black
                gameLoop(newBoard, nextLstOpenCoords, nextPlayer, newR, lines, cols)
        }
    }

    ///====In-Game Functions====

    //choose a random move to do
    def randomMove(lstOpenCoords: List[Coord2D], rand: RandomWithState): (Coord2D, RandomWithState) = {
       val (randPos, newRand) = rand.nextInt(lstOpenCoords.length)  //choose random pos in lstOpenCoords
       val chosenCoord = lstOpenCoords(randPos)  //gets the correspondent coord
       (chosenCoord, newRand)
    }

    //does a basic play
    def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
        val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)
        if(!validPlay(board, coordFrom, coordTo, coordMiddle, player)){ (None, lstOpenCoords)
        }else{
            val newBoard = board - coordFrom - coordMiddle + (coordTo -> player)    //Update the board
            val addToLstOpenCoords = coordFrom::coordMiddle::lstOpenCoords          //Add coordFrom and coordMiddle to lstOpenCoords
            val removeToOpenCoords = addToLstOpenCoords.filter(c => c != coordTo)   //Remove coordTo to lstOpenCoords
            (Some(newBoard), removeToOpenCoords)     
        }
    }
        //aux to play
    def validPlay(board: Board, coordFrom:Coord2D, coordTo:Coord2D, coordMiddle: Coord2D, player:Stone):Boolean = {
        val range = Math.abs(coordFrom._1 - coordTo._1) + Math.abs(coordFrom._2 - coordTo._2)    //Range to move pieces
        val opponent = if(player == Stone.Black) Stone.White else Stone.Black                   //Determinate opponent's piece

        val rangeOk = range == 2                                                                //Range to move pieces
        val validStoneToPlay = board.get(coordFrom) == Some(player)                             //coordFrom isn't empty
        val validPosToGo = board.get(coordTo) == None                                           //coordTo is empty
        val validOppMid = board.get(coordMiddle) == Some(opponent)                              //coordMiddle has an opponent

        rangeOk && validStoneToPlay && validPosToGo && validOppMid && (coordFrom._1 == coordTo._1 || coordFrom._2 == coordTo._2)
    }                                                                  //makes it impossible to jump in diagonals

    //Does a random play                                                                            //f equals to randomMove(...)
    def playRandomly(board: Board, r: RandomWithState, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], RandomWithState) => (Coord2D, RandomWithState)): (Option[Board], RandomWithState, List[Coord2D], Option[Coord2D], Option[Coord2D]) = {   
        val validCoords = validTargets(board, lstOpenCoords, player)    //find coordTo(empty spaces) that can be played
        
        if(validCoords.isEmpty){    //No coordTo to be played to, end of game
            (None, r, Nil, None, None)
        } else {
            val(chosenCoordTo, newR1) = f(validCoords, r)   //chooses a random coordTo
            val playablePieces = validSources(board, chosenCoordTo, player) //find pieces that can go to coordTo
            val (chosenCoordFrom, newR2) = f(playablePieces, newR1) //chooses a random coordFrom
            val (newBoard, newLstOpenCoords) = play(board, player, chosenCoordFrom, chosenCoordTo, lstOpenCoords)   //plays

            (newBoard, newR2, newLstOpenCoords, Some(chosenCoordFrom), Some(chosenCoordTo))
        }
    }
    //aux to playRandomly - returns all the coords that can go to a coord
    def findPossiblePlays(coord: Coord2D): List[Coord2D] = {
        val (line, col) = coord
        List(
            (line - 2, col),
            (line + 2, col),
            (line, col - 2),
            (line, col + 2),
        )
    }
    //aux to playRandomly - find all valid coordFrom that can go to coordTo(empty space)
    def validSources(board:Board, coordTo:Coord2D, player:Stone):List[Coord2D] = {
        findPossiblePlays(coordTo).filter {coordFrom =>
            val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)
            validPlay(board, coordFrom, coordTo, coordMiddle, player)}
    }
    //aux to playRandomly - filter the list keeping just the coordTo that can be played to
    def validTargets(board:Board, coords:List[Coord2D], player:Stone):List[Coord2D] = coords match {
    case Nil => Nil //base case: there are no coordTo
        case coordTo::tail =>   //case coordTo has valid moves, stay in the list
            if(validSources(board, coordTo, player).nonEmpty)
                coordTo::validTargets(board, tail, player)
            else
                validTargets(board, tail, player)
    }
    

    //====Initialize Board====

    //Generate pieces for board
    def generatePieces(lines:Int, cols:Int, maxLines:Int, maxCols:Int):List[(Coord2D, Stone)] = {
        if(lines >= maxLines) Nil //Base Case: Lines ended
        else if(cols >= maxCols) generatePieces(lines + 1, 0, maxLines, maxCols) //Next line
        else {
            val stone = if((lines + cols) % 2 == 0) Stone.Black else Stone.White
            ((lines, cols), stone)::generatePieces(lines, cols + 1, maxLines, maxCols) //Add pieces and go next col
        }
    }

    //Initiates board
    def initBoard(lines:Int, cols:Int, r:RandomWithState):(Board, RandomWithState, List[Coord2D]) = { //Init Board
        val piecesList = generatePieces(0, 0, lines, cols) //Build board
        val allCoords = piecesList.map(_._1) //Return list with all coordenates

        val(randomIndex1, r1) = r.nextInt(allCoords.length)     //Pick a random piece
        val piece1 = allCoords(randomIndex1)

        val adjacents = getAdjacentCoord(piece1, lines, cols)   //Pick a random piece between the adjacents
        val(randomIndex2, r2) = r1.nextInt(adjacents.length)
        val piece2 = adjacents(randomIndex2)

        val board = piecesList.toMap - piece1 - piece2  //Remove both pieces
        (board, r2, List(piece1, piece2))
    }

    //Returns all adjacent coords
    def getAdjacentCoord(coord:Coord2D, lines:Int, cols:Int):List[Coord2D] = {
        val (l, c) = coord
        List((l - 1, c), (l + 1, c), (l, c - 1), (l, c + 1)).filter{ case (x, y) => x >= 0 && x < lines && y >= 0 && y < cols }
    }

}