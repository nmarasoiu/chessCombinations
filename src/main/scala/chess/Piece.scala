package chess

import enumeratum.{Enum, EnumEntry}
import scalaz.Memo

import scala.collection.immutable
import scala.collection.immutable.BitSet

sealed abstract class Piece(val pieceIndex: Int) extends EnumEntry with Ordered[Piece] {
  def compare(that: Piece): Int = pieceIndex - that.pieceIndex

  val incompatiblePositions: PositionInTable => PositionSet =
    Memo.immutableHashMapMemo[PositionInTable, PositionSet] {
      positionInTable =>
        val table: Table = positionInTable.table
        val position: Position = positionInTable.position
        val positions =
          for (xy <- incompatiblePositions(position.x(table), position.y(table), table))
            yield Position(xy.x, xy.y, table).positionInt
        PositionSet(BitSet(positions: _*))
    }

  /**
    * @return the BitSet of positions that cannot be filled by other pieces:
    *         this BitSet of positions includes this piece position itself,
    *         as well as all the positions on the table which this piece can attack
    *         and therefore cannot be occupied by other pieces part of this solution
    */
  protected def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY]
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  def of(pieceIdx: Int): Piece = values(pieceIdx)

  case object Queen extends Piece(0) {
    override def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY] =
      Rook.incompatiblePositions(x, y, table) ++ Bishop.incompatiblePositions(x, y, table)
  }

  case object Bishop extends Piece(1) {
    override def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY] = {
      val h = table.horizontal.length
      for (hOffset <- 1 - h until h;
           possibleXY = XY(x + XOffset(hOffset), y + YOffset(hOffset))
           if possibleXY.fitsIn(table))
        yield possibleXY
    }
  }

  case object Rook extends Piece(2) {
    override def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY] = {
      val xs =
        for (hOffset <- 0 until table.horizontal.length)
          yield XY(X(hOffset), y)
      val ys =
        for (vOffset <- 0 until table.vertical.height)
          yield XY(x, Y(vOffset))
      xs ++ ys
    }
  }

  case object Knight extends Piece(3) {
    private val horizontalVerticalOffsets: Array[(XOffset, YOffset)] =
      for ((absHorizontalOffset, absVerticalOffset) <-
             Array((XOffset(0), YOffset(0)), (XOffset(1), YOffset(2)), (XOffset(2), YOffset(1)));
           (hOffset, vOffset) <- Set(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield (hOffset, vOffset)

    override def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY] = {
      for ((hOffset, vOffset) <- horizontalVerticalOffsets;
           possibleXY = XY(x + hOffset, y + vOffset) if possibleXY.fitsIn(table))
        yield possibleXY
    }
  }

  val xOne: XOffset = XOffset.one
  val yOne: YOffset = YOffset.one

  case object King extends Piece(4) {
    override def incompatiblePositions(x: X, y: Y, table: Table): Seq[XY] = {
      for (x <- Seq(x - xOne, x, x + xOne) if x.fitsIn(table);
           y <- Seq(y - yOne, y, y + yOne) if y.fitsIn(table)) yield XY(x, y)
    }
  }
}
