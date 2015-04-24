import java.io.Serializable

/**
 * Created by peterprokop on 31/03/15.
 */

sealed trait PieceColor extends Serializable {
  def oppositeColor:PieceColor = if (this==White) {
    Black
  } else {
    White
  }
}
case object White extends PieceColor
case object Black extends PieceColor

sealed abstract class PieceType(val repr: Char) extends Serializable {
  val values = Map('p' -> 1, 'n' -> 3, 'b' -> 3, 'r' -> 5, 'q' -> 9, 'k' -> 1000000)
  val value: Int = values(repr)

  override def toString() = repr.toString
}

object PieceType extends Serializable {
  case object King extends PieceType('k')
  case object Queen extends PieceType('q')
  case object Rook extends PieceType('r')
  case object Bishop extends PieceType('b')
  case object Knight extends PieceType('n')
  case object Pawn extends PieceType('p')

  def typeFromChar(repr: Char): PieceType = {
    repr match {
      case 'k' => King
      case 'q' => Queen
      case 'r' => Rook
      case 'b' => Bishop
      case 'n' => Knight
      case 'p' => Pawn
      case c => throw new IllegalArgumentException()
    }
  }
}

class Piece(repr: Char, val color: PieceColor) extends Serializable {
  val pieceType = PieceType.typeFromChar(repr)

  override def toString() = if (color == White) {
    pieceType.toString.toUpperCase
  } else {
    Console.BLUE + pieceType.toString + Console.RESET
  }
}