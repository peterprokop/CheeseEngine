import java.io.Serializable

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
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
    val numOfMoves = possibleMoves.size
    if (numOfMoves == 0) {
      null
    } else {
      possibleMoves(Random.nextInt(numOfMoves))
    }
  }
}

object NegaMax extends Serializable {
  val defaultDepth = 3

  def materialScore(game: Game, color: PieceColor) = {
    val board = game.board
    board.materialOfColor(color) - board.materialOfColor(color.oppositeColor)
  }

  def mobilityScore(game: Game, color: PieceColor) = {
    val board = game.board
    board.possibleMovesForColor(color).length - board.possibleMovesForColor(color.oppositeColor).length
  }

  def evaluateDefault(game: Game, color: PieceColor) = {
    materialScore(game, color) + 0.1 * mobilityScore(game, color)
  }

  def negaMax(game: Game, color: PieceColor, depth: Int): (Move, Double) = {
    val board = game.board

    if (depth == 0) {
      (null, evaluateDefault(game, color))
    } else {
      var moveCandidate: Move = null
      var moveCandidateScore = Double.NegativeInfinity

      for (move <- board.possibleMovesForColor(color)) {
        val candidate = negaMax(new Game(board.boardByMakingMove(move), color.oppositeColor, null), color.oppositeColor, depth - 1)
        val score = -candidate._2

        if (score > moveCandidateScore) {
          moveCandidateScore = score
          moveCandidate = move
        }
      }

      (moveCandidate, moveCandidateScore)
    }
  }
}

class NegaMaxSpark(sc: SparkContext) extends Serializable  {
  val movesOrdering = new Ordering[Tuple2[Move, Double]]() {
    override def compare(x: (Move, Double), y: (Move, Double)): Int =
      Ordering[Double].compare(x._2, y._2)
  }

  def negaMaxSpark(game: Game, color: PieceColor, depth: Int): (Move, Double) = {
    val board = game.board

    if (depth == 0) {
      (null, NegaMax.evaluateDefault(game, color))
    } else {
      val moves = board.possibleMovesForColor(color)
      val movesPar = sc.parallelize(moves)

      val moveMappingFunc = (m: Move) => { NegaMax.negaMax(new Game(board.boardByMakingMove(m), color.oppositeColor, null), color.oppositeColor, depth - 1) }
      val movesWithScorePar = movesPar.map(m => (m, moveMappingFunc(m)._2))
      val move = movesWithScorePar.min()(movesOrdering)

      (move._1, -move._2)
    }
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

class NegaMaxBot(val maxDepth: Int) extends Bot {
  def nextMove(game: Game): Move = {
    NegaMax.negaMax(game, game.colorToMove, maxDepth)._1
  }
}

class NegaMaxSparkBot(val maxDepth: Int, sc: SparkContext) extends Bot {
  def nextMove(game: Game): Move = {
    val nms = new NegaMaxSpark(sc)
    nms.negaMaxSpark(game, game.colorToMove, maxDepth)._1
  }
}