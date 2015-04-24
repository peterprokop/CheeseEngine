import scala.util.Random

/**
 * Created by peterprokop on 18/04/15.
 */

class Game(val board: Board, val colorToMove: PieceColor, val bots:Map[PieceColor, Bot]) {
  def nextMove(): (Game, Move) = {
    val move = bots(colorToMove).nextMove(this)
    (new Game(board.boardByMakingMove(move), colorToMove.oppositeColor, bots), move)
  }

  def gameAfterNumberOfMoves(numberOfMoves: Int): Game = {
    var currentGame = this
    for (i <- 1 to numberOfMoves) {
      val (newGame, nextMove) = currentGame.nextMove()

      val b = currentGame.board
      println(currentGame.toString(nextMove))
      println()
      println("Material W:" + b.materialOfColor(White) + " B:" + b.materialOfColor(Black))
      println("------")
      println(nextMove)

      currentGame = newGame
    }

    currentGame
  }

  def toString(move: Move): String = {
    val columnLetters = 'a' to 'z'
    var output = ""

    for (row <- (0 until board.height).reverse) {
      output += ((row+1).toString + " ")

      for (col <- 0 until board.width) {
        val spot = board.pieces(col)(row)

        spot match {
          case None => output += " "
          case Some(p) => if (move.to.r == row && move.to.c == col) {
            output += Console.BLACK_B + p.toString() + Console.RESET
          } else if (move.from.r == row && move.from.c == col) {
            output += Console.BLACK_B + p.toString() + Console.RESET
          } else {
            output += p.toString()
          }
        }
      }

      output += "\n"
    }

    output += "  "
    for (col <- 0 until board.width) {
      output += columnLetters(col)
    }

    output
  }
}

