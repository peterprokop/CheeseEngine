import PieceType._

/**
 * Created by peterprokop on 31/03/15.
 */

object Board {
  val defaultBoardSize = 8

  def defaultBoard: Board = defaultBoardWithSize(defaultBoardSize, defaultBoardSize)

  def defaultBoardWithSize(width: Int, height: Int): Board = {
    val array = Array.ofDim[Option [Piece] ](height, width)

    for (row <- 0 until height) {
      for (col <- 0 until width) {
        array(row)(col) = None
      }
    }

    val whiteMapper   = { x: Char => new Piece(x, White) }
    val blackMapper   = { x: Char => new Piece(x, Black) }
    val someMapper    = { x: Piece => Some(x) }

    val row1 = "rnbqkbnr".toArray.map(whiteMapper)
    val row2 = "pppppppp".toArray.map(whiteMapper)
    val row7 = "pppppppp".toArray.map(blackMapper)
    val row8 = "rnbqkbnr".toArray.map(blackMapper)

    array(0) = row1.map(someMapper)
    array(1) = row2.map(someMapper)
    array(6) = row7.map(someMapper)
    array(7) = row8.map(someMapper)

    val pieces = array.transpose
    new Board(width, height, pieces)
  }
}

class Position(val c: Int,
               val r: Int
               ) extends Serializable
{
  override def toString() = {
    val columns = 'a' to 'z'
    columns(c).toString + (r+1).toString
  }
}

class Move(val from: Position,
           val to: Position) extends Serializable
{
  def this(r1: Int, c1: Int, r2: Int, c2 : Int) = this(new Position(r1, c1), new Position(r2, c2))

  override def toString() = from.toString + " " + to.toString
}

class Board(val width: Int, val height: Int, val pieces: Array[Array[Option[Piece]]]) extends Serializable {
  def isPositionValid(position: Position) : Boolean = {
    if (position.c < 0 || position.r < 0) {
      false
    } else if (position.c >= width || position.r >= height) {
      false
    } else {
      true
    }
  }

  def canMoveToPosition(position: Position, color: PieceColor): Boolean = {
    pieces(position.c)(position.r) match {
      case Some(pc) => (pc.color != color);
      case _ => true;
    }
  }

  def isEnemyPresentAtPosition(position: Position, color: PieceColor): Boolean = {
    pieces(position.c)(position.r) match {
      case Some(pc) => (pc.color != color);
      case _ => false;
    }
  }

  def piecesOfColor(color: PieceColor): Seq[(Piece, Position)] = {
    for {
      row <- 0 until height
      col <- 0 until width
      p <- pieces(col)(row)
      if p.color == color
    } yield (p, new Position(col, row))
  }

  def materialOfColor(color: PieceColor): Int = {
    piecesOfColor(color).foldLeft(0)((sum, t) => sum + t._1.pieceType.value) - King.value
  }

  def boardByMakingMove(move: Move): Board = {
    // TODO: validate move
    val pieceToMove = pieces(move.from.c)(move.from.r)

    val pieceMoved = pieces.updated(move.to.c, pieces(move.to.c).updated(move.to.r, pieceToMove))
    val newPieces = pieceMoved.updated(move.from.c, pieceMoved(move.from.c).updated(move.from.r, None))

    return new Board(width, height, newPieces)
  }

  def getFreeSpacesInDirection(position: Position, dc: Int, dr: Int, color: PieceColor): List[Position] = {
    val incrementPosition = new Position(position.c + dc, position.r + dr)
    if (isPositionValid(incrementPosition) && canMoveToPosition(incrementPosition, color)) {
      if (isEnemyPresentAtPosition(incrementPosition, color)) {
        incrementPosition :: Nil
      } else {
        incrementPosition :: getFreeSpacesInDirection(incrementPosition, dc, dr, color)
      }
    } else {
      Nil
    }
  }

  def possibleMovesForColor(color: PieceColor): Seq[Move] = {
    // TODO: king's check, pawn promotion, castling, en passant, threefold repetition, 50-move rule
    val moves = for {
      (piece, oldPosition) <- piecesOfColor(color)
    } yield {
      val direction = if (color == White) {1} else {-1}
      piece.pieceType match {
        case Pawn => {
          val oneCell = List(new Position(oldPosition.c, oldPosition.r + direction)).filter(p => isPositionValid(p) && canMoveToPosition(p, color) && !isEnemyPresentAtPosition(p, color))
          val twoCells = if (oneCell.size == 1 && ((color == White && oldPosition.r == 1) || (color == Black && oldPosition.r == height - 2))) {
            List(new Position(oldPosition.c, oldPosition.r + direction*2)).filter(p => isPositionValid(p) && canMoveToPosition(p, color) && !isEnemyPresentAtPosition(p, color))
          } else { List() }

          val attack = List(
            new Position(oldPosition.c + 1, oldPosition.r + direction),
            new Position(oldPosition.c - 1, oldPosition.r + direction)
          ).filter(p => isPositionValid(p) && isEnemyPresentAtPosition(p, color))

          val candidates = oneCell ++ twoCells ++ attack
          candidates.map(x => new Move(oldPosition, x))

        }
        case Knight => {
          val candidates = List(
            new Position(oldPosition.c + 1, oldPosition.r + 2),
            new Position(oldPosition.c + 1, oldPosition.r - 2),
            new Position(oldPosition.c + 2, oldPosition.r + 1),
            new Position(oldPosition.c + 2, oldPosition.r - 1),
            new Position(oldPosition.c - 1, oldPosition.r + 2),
            new Position(oldPosition.c - 1, oldPosition.r - 2),
            new Position(oldPosition.c - 2, oldPosition.r + 1),
            new Position(oldPosition.c - 2, oldPosition.r - 1)
          )

          candidates.filter(x => isPositionValid(x) && canMoveToPosition(x, color)).map(x => new Move(oldPosition, x))
        }
        case Rook => {
          val horizontalUp = getFreeSpacesInDirection(oldPosition, 1, 0, color)
          val horizontalDown = getFreeSpacesInDirection(oldPosition, -1, 0, color)

          val verticalRight = getFreeSpacesInDirection(oldPosition, 0, 1, color)
          val verticalLeft = getFreeSpacesInDirection(oldPosition, 0, -1, color)

          val candidates = horizontalUp ++ horizontalDown ++ verticalLeft ++ verticalRight
          candidates.map(x => new Move(oldPosition, x))
        }
        case King => {
          val candidates = for {
            col <- -1 to 1
            row <- -1 to 1
            if !(col == 0 && row == 0)
          } yield {
            new Position(oldPosition.c + col, oldPosition.r + row)
          }

          candidates.filter(x => isPositionValid(x) && canMoveToPosition(x, color)).map(x => new Move(oldPosition, x))
        }
        case _ => {
          List()
        }
      }
    }

    moves.flatten
  }

  override def toString(): String = {
    val columnLetters = 'a' to 'z'
    var output = ""

    for (row <- (0 until height).reverse) {
      output += ((row+1).toString + " ")

      for (col <- 0 until width) {
        val spot = pieces(col)(row)

        spot match {
          case None => output += " "
          case Some(p) => output += p.toString()
        }

      }

      output += "\n"
    }

    output += "  "
    for (col <- 0 until width) {
      output += columnLetters(col)
    }

    output
  }
}
