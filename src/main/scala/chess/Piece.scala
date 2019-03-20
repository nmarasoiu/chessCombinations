package chess

import enumeratum.{Enum, EnumEntry}
import org.roaringbitmap.RoaringBitmap
import scalaz.Memo

import scala.collection.immutable

sealed abstract class Piece(val order: Int) extends EnumEntry with Ordered[Piece] {
  val incompatiblePositions: PositionInTable => Positions =
    Memo.immutableHashMapMemo[PositionInTable, Positions] {
      case PositionInTable(position: Position, table: Table) =>
        val (x, y) = table.fromIntToPair(position)
        val positions: Seq[Position] =
          for ((x, y) <- incompatiblePositions(x, y, table))
            yield table.fromPairToInt(x, y)
        RoaringBitmap.bitmapOfUnordered(positions: _*)
    }

  def compare(that: Piece): Int = this.order - that.order

  /**
    * @return the BitSet of positions that cannot be filled by other pieces:
    *         this BitSet of positions includes this piece position itself,
    *         as well as all the positions on the table which this piece can attack
    *         and therefore cannot be occupied by other pieces part of this solution
    */
  protected def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)]
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  def of(ordinal: Int): Piece = values(ordinal)

  def fittingXY(table: Table)(position: (Int, Int)): Boolean = {

    def fittingX(table: Table)(x: Int): Boolean = 0 <= x && x < table.horizontal

    def fittingY(table: Table)(y: Int): Boolean = 0 <= y && y < table.vertical

    fittingX(table)(position._1) && fittingY(table)(position._2)
  }

  case object Queen extends Piece(0) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] =
      Rook.incompatiblePositions(x, y, table) ++ Bishop.incompatiblePositions(x, y, table)
  }

  case object Bishop extends Piece(1) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      val h = table.horizontal
      for (hOffset <- 1 - h until h if fittingXY(table)(x + hOffset, y + hOffset))
        yield (x + hOffset, y + hOffset)
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
  }

  case object Knight extends Piece(3) {
    private val horizontalVerticalOffsets: Array[(Int, Int)] =
      for ((absHorizontalOffset, absVerticalOffset) <- Array((0, 0), (1, 2), (2, 1));
           (hOffset, vOffset) <- Set(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield (hOffset, vOffset)

    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      for ((hOffset, vOffset) <- horizontalVerticalOffsets if fittingXY(table)(x + hOffset, y + vOffset))
        yield (x + hOffset, y + vOffset)
    }
  }

  case object King extends Piece(4) {
    override def incompatiblePositions(x: Int, y: Int, table: Table): Seq[(Int, Int)] = {
      val xs = math.max(0, x - 1) to math.min(x + 1, table.horizontal - 1)
      val ys = math.max(0, y - 1) to math.min(y + 1, table.vertical - 1)
      for (x <- xs; y <- ys) yield (x, y)
    }
  }

}
