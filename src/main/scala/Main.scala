import org.apache.spark.{SparkContext, SparkConf}

/**
 * Created by peterprokop on 31/03/15.
 */

// Can be launched as an usual scala program

object Main {
  import org.apache.spark.SparkContext
  import org.apache.spark.SparkConf

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("Cheese Engine").setMaster("local")
    val sc = new SparkContext(conf)

    val board = Board.defaultBoard
    val bots: Map[PieceColor, Bot] = Map(White -> new NegaMaxBot(2),
      Black -> new NegaMaxSparkBot(4, sc))
    val game = new Game(board, White, bots)
    game.gameAfterNumberOfMoves(100)
  }
}

/*
 Should be launched by spark:
 sbt package && SPARK_HOME/bin/spark-submit --class "Spark" --master local[*] target/scala-2.10/cheeseengine_2.10-1.0.jar > log.txt
 */

object Spark {
  import org.apache.spark.SparkContext
  import org.apache.spark.SparkConf

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("Cheese Engine")
    val sc = new SparkContext(conf)

    val board = Board.defaultBoard
    val bots: Map[PieceColor, Bot] = Map(White -> new RandomBot(),
      Black -> new NegaMaxSparkBot(4, sc))
    val game = new Game(board, White, bots)
    game.gameAfterNumberOfMoves(100)
  }
}