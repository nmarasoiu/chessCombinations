package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable
import scala.math.abs

sealed abstract class Piece(val order: Int,
                            protected val attackPositions: Function[(Position, Table), Seq[Position]],
                           ) extends EnumEntry with Ordered[Piece] {
  def takes(piecePosition: Position, otherPosition: Position): Boolean

  val incompatiblePositions: Function[(Position, Table), Seq[Position]] = {
    case (pos, table@Table(h, v)) =>
      attackPositions(pos, table).filter {
        case Position(x, y) => 0 <= x && x < h && 0 <= y && y <= v
      }
  }

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object King extends Piece(1, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- -1 to 1 if x + hOffset >= 0;
           vOffset <- -1 to 1 if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  }) {
    override def takes(piecePosition: Position, otherPosition: Position): Boolean = {
      abs(piecePosition.x - otherPosition.x) <= 1 &&
        abs(piecePosition.y - otherPosition.y) <= 1
    }
  }

  case object Queen extends Piece(2,
    a => Rook.attackPositions(a) ++ Bishop.attackPositions(a)) {
    override val incompatiblePositions: Function[(Position, Table), Seq[Position]] = attackPositions

    override def takes(piecePosition: Position, otherPosition: Position): Boolean =
      Rook.takes(piecePosition, otherPosition) || Bishop.takes(piecePosition, otherPosition)
  }

  //nebun
  case object Bishop extends Piece(3, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- (1 - h until h).toStream if 0 <= x + hOffset && 0 <= y + hOffset)
        yield Position(x + hOffset, y + hOffset)
  }) {
    override def takes(piecePosition: Position, otherPosition: Position): Boolean = {
      abs(piecePosition.x - otherPosition.x) == abs(piecePosition.y - otherPosition.y)
    }
  }

  val horizontalVerticalOffsets: Seq[(Int, Int)] =
    for ((absHorizontalOffset, absVerticalOffset) <- Seq((1, 2), (2, 1));
         (hOffset, vOffset) <- Seq(
           (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
           (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
      yield (hOffset, vOffset)

  //cal
  case object Knight extends Piece(0, {
    case (Position(x, y), Table(h, v)) =>
      for ((hOffset, vOffset) <- horizontalVerticalOffsets if x + hOffset >= 0 && 0 <= y + vOffset)
        yield Position(x + hOffset, y + vOffset)
  }) {
    override val incompatiblePositions: Function[(Position, Table), Seq[Position]] = attackPositions

    override def takes(piecePosition: Position, otherPosition: Position): Boolean = {
      val tuple = (abs(piecePosition.x - otherPosition.x), abs(piecePosition.y - otherPosition.y))
      (1, 2) == tuple || (2, 1) == tuple
    }
  }

  //tura
  case object Rook extends Piece(4, {
    case (Position(x, y), Table(h, v)) =>
      Stream(
        for (hOffset <- (0 until h).toStream) yield Position(hOffset, y),
        for (vOffset <- (0 until v).toStream) yield Position(x, vOffset)).flatten
  }) {
    override def takes(piecePosition: Position, otherPosition: Position): Boolean = {
      piecePosition.x == otherPosition.x || piecePosition.y == otherPosition.y
    }
  }

}
