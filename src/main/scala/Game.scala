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
      val b = currentGame.board
      println(currentGame.board)
      println()
      println("Material W:" + b.materialOfColor(White) + " B:" + b.materialOfColor(Black))
      println("------")

      val (newGame, nextMove) = currentGame.nextMove()
      println(nextMove)
      currentGame = newGame
    }

    currentGame
  }
}

