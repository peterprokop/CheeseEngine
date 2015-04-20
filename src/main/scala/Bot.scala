import java.io.Serializable

import org.apache.spark.SparkContext
import scala.util.Random

/**
 * Created by peterprokop on 20/04/15.
 */

abstract class Bot {
  def nextMove(game: Game): Move
}

class RandomBot extends Bot {
  def nextMove(game: Game): Move = {
    val possibleMoves = game.board.possibleMovesForColor(game.colorToMove)
    possibleMoves(Random.nextInt(possibleMoves.size))
  }
}

class SparkBot(sc: SparkContext) extends Bot {
  def nextMove(game: Game): Move = {
    val board = game.board
    val color = game.colorToMove
    val moves = board.possibleMovesForColor(color)
    val movesPar = sc.parallelize(moves)

    val materialFunc =  (m: Move) => { -board.boardByMakingMove(m).materialOfColor(color.oppositeColor) }

    val movesWithMaterial = movesPar.map(m => (m, materialFunc(m)))
    val materialMax = movesWithMaterial.map(_._2).max()
    val optimalMoves = movesWithMaterial.filter(_._2 == materialMax)

    val count = optimalMoves.count().toInt
    val index = Random.nextInt(count)
    optimalMoves.collect()(index)._1
  }
}