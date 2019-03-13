package chess

import enumeratum.{Enum, EnumEntry}
import scalaz.Memo

import scala.collection.immutable
import scala.collection.immutable.BitSet
import scala.math.abs

sealed abstract class Piece(val order: Int) extends EnumEntry with Ordered[Piece] {

  /**
    * Returns true if this Piece, situated at piecePosition, can take out another piece situated at otherPosition
    */
  def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean
//    incompatiblePositions(piecePosition,table)(Position.fromPairToInt(otherPosition))

  type KIntIntTable = ((Int, Int), Table)
  val incompatiblePositions: KIntIntTable => Positions =
    Memo.immutableHashMapMemo[KIntIntTable, Positions] {
      case ((x: Int, y: Int), table: Table) =>
        val positions = for ((x, y) <- incompatiblePositions(x, y, table)) yield Position.fromPairToInt(x, y, table)
        BitSet(positions: _*)
    }

  /**
    * @return the BitSet of positions that cannot be filled by other pieces:
    *         this BitSet of positions includes this piece position itself,
    *         as well as all the positions on the table which this piece can attack
    *         and therefore cannot be occupied by other pieces part of this solution
    */
  protected def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)]

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object Queen extends Piece(0) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] =
      Rook.incompatiblePositions(x, y, table) ++ Bishop.incompatiblePositions(x, y, table) //todo bitSet |

    override def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean =
      Rook.takes(piecePosition, otherPosition, table) || Bishop.takes(piecePosition, otherPosition, table)
  }

  case object Bishop extends Piece(1) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      val h = table.horizontal
      for (hOffset <- 1 - h until h if fittingXY(table)(x + hOffset, y + hOffset))
        yield (x + hOffset, y + hOffset)
    }

    override def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) == abs(y - y2)
      }
  }

  case object Rook extends Piece(2) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      val xs =
        for (hOffset <- 0 until table.horizontal)
          yield (hOffset, y)
      val ys =
        for (vOffset <- 0 until table.vertical)
          yield (x, vOffset)
      xs ++ ys
    }

    override def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean =
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

    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      for ((hOffset, vOffset) <- horizontalVerticalOffsets if fittingXY(table)(x + hOffset, y + vOffset))
        yield (x + hOffset, y + vOffset)
    }

    override def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          def betweenOneAndTwo(x: Int): Boolean = 1 <= x && x <= 2

          val xDiff = abs(x - x2)
          val yDiff = abs(y - y2)
          xDiff != yDiff && betweenOneAndTwo(xDiff) && betweenOneAndTwo(yDiff)
      }
  }

  case object King extends Piece(4) {

    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      val xs = math.max(0, x - 1) to math.min(x + 1, table.horizontal - 1)
      val ys = math.max(0, y - 1) to math.min(y + 1, table.vertical - 1)
      for (x <- xs; y <- ys) yield (x, y)
    }

    override def takes(piecePosition: (Position, Position), otherPosition: (Position, Position), table: Table): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) <= 1 && abs(y - y2) <= 1
      }
  }

  def fittingXY(table: Table)(position: (Int, Int)): Boolean = {

    def fittingX(table: Table)(x: Int): Boolean = 0 <= x && x < table.horizontal

    def fittingY(table: Table)(y: Int): Boolean = 0 <= y && y < table.vertical

    fittingX(table)(position._1) && fittingY(table)(position._2)
  }
}
