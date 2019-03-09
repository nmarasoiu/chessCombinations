package chess

import enumeratum.{Enum, EnumEntry}
import scalaz.Memo

import scala.collection.immutable
import scala.collection.immutable.BitSet
import scala.math.abs
import Position._

sealed abstract class Piece(val order: Int) extends EnumEntry with Ordered[Piece] {
  final def takes(piecePosition: Int, otherPosition: Int, table: Table): Boolean =
    takes(Position.fromIntToPair(piecePosition, table), Position.fromIntToPair(otherPosition, table))

  /**
    * Returns true if this Piece, situated at piecePosition, can take out another piece situated at otherPosition
    */
  def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean

  def incompatiblePositions(position: Position, table: Table): Positions = {
    val pair = Position.fromIntToPair(position, table)
    incompatiblePositions.apply(pair._1, pair._2, table)
  }

  type KIntIntTable = (Int, Int, Table)
  final val incompatiblePositions: KIntIntTable => Positions =
    Memo.immutableHashMapMemo[KIntIntTable, Positions] {
      case (x: Int, y: Int, t: Table) => incompatiblePositions(x, y, t)
    }

  /**
    * @return the BitSet of positions that cannot be filled by other pieces:
    *         this BitSet of positions includes this piece position itself,
    *         as well as all the positions on the table which this piece can attack
    *         and therefore cannot be occupied by other pieces part of this solution
    */
  def incompatiblePositions(x: Int, y: Int, table: Table): Positions

  final def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object Queen extends Piece(0) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Positions =
      Rook.incompatiblePositions(x, y, table) | Bishop.incompatiblePositions(x, y, table)

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      Rook.takes(piecePosition, otherPosition) || Bishop.takes(piecePosition, otherPosition)
  }

  case object Bishop extends Piece(1) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Positions = {
      val h = table.horizontal
      val xys: IndexedSeq[Int] =
        for (hOffset <- 1 - h until h if fittingXY(table)(x + hOffset, y + hOffset))
          yield fromPairToInt(x + hOffset, y + hOffset, table)

      BitSet(xys: _*)
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) == abs(y - y2)
      }
  }

  case object Rook extends Piece(2) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Positions = {
      val xs: Seq[Int] =
        for (hOffset <- 0 until table.horizontal)
          yield fromPairToInt(hOffset, y, table)
      val ys: Seq[Int] =
        for (vOffset <- 0 until table.vertical)
          yield fromPairToInt(x, vOffset, table)
      BitSet(xs ++ ys: _*)
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) => x == x2 || y == y2
      }
  }

  case object Knight extends Piece(3) {
    val horizontalVerticalOffsets: Array[(Int, Int)] =
      for ((absHorizontalOffset, absVerticalOffset) <- Array((0, 0), (1, 2), (2, 1));
           (hOffset, vOffset) <- Set(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield (hOffset, vOffset)

    override def incompatiblePositions(x: Int, y: Int, table: Table): Positions = {
      val xys: Seq[Int] =
        for ((hOffset, vOffset) <- horizontalVerticalOffsets if fittingXY(table)(x + hOffset, y + vOffset))
          yield fromPairToInt(x + hOffset, y + vOffset, table)
      BitSet(xys: _*)
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          val tuple = (abs(x - x2), abs(y - y2))
          (1, 2) == tuple || (2, 1) == tuple
      }
  }

  case object King extends Piece(4) {

    override def incompatiblePositions(x: Int, y: Int, table: Table): Positions = {
      val xs: IndexedSeq[Int] = math.max(0, x - 1) to math.min(x + 1, table.horizontal - 1)
      val ys: IndexedSeq[Int] = math.max(0, y - 1) to math.min(y + 1, table.vertical - 1)
      val xys: IndexedSeq[Int] = for (x <- xs; y <- ys) yield Position.fromPairToInt(x, y, table)
      BitSet(xys: _*)
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) <= 1 && abs(y - y2) <= 1
      }
  }

  final def fittingX(table: Table)(x: Int): Boolean = 0 <= x && x < table.horizontal

  final def fittingY(table: Table)(y: Int): Boolean = 0 <= y && y < table.vertical

  final def fittingXY(table: Table)(position: (Int,Int)): Boolean = fittingX(table)(position._1) && fittingY(table)(position._2)

}
