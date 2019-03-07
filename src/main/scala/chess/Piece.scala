package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable.BitSet
import scala.collection.{immutable, mutable}
import scala.math.abs

sealed abstract class Piece(val order: Int) extends EnumEntry with Ordered[Piece] {
  final def attackPositions(position: Position, table: Table): Positions = attackPositions(position.x, position.y)(table)

  final def takes(piecePosition: Position, otherPosition: Position): Boolean = takes(piecePosition.pair, otherPosition.pair)

  def attackPositions(x: Int, y: Int)(table: Table): Positions

  def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean

  final def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object Queen extends Piece(0) {
    override def attackPositions(x: Int, y: Int)(table: Table): Positions =
      Rook.attackPositions(x, y)(table) | Bishop.attackPositions(x, y)(table)

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      Rook.takes(piecePosition, otherPosition) || Bishop.takes(piecePosition, otherPosition)
  }

  case object Bishop extends Piece(1) {
    override def attackPositions(x: Int, y: Int)(table: Table): Positions = {
      build({ set =>
        val h = table.horizontal
        for (hOffset <- 1 - h until h if fittingXY(table)(Position(x + hOffset, y + hOffset))) {
          set += Position(x + hOffset, y + hOffset).xy
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) == abs(y - y2)
      }
  }

  case object Rook extends Piece(2) {
    override def attackPositions(x: Int, y: Int)(table: Table): Positions = {
      build({ set =>
        for (hOffset <- 0 until table.horizontal) {
          set += Position(hOffset, y).xy
        }
        for (vOffset <- 0 until table.vertical) {
          set += Position(x, vOffset).xy
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) => x == x2 || y == y2
      }
  }

  case object Knight extends Piece(3) {
    val horizontalVerticalOffsets: Array[(Int, Int)] =
      for ((absHorizontalOffset, absVerticalOffset) <- Array((1, 2), (2, 1));
           (hOffset, vOffset) <- Seq(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield (hOffset, vOffset)

    override def attackPositions(x: Int, y: Int)(table: Table): Positions = {
      build({ set =>
        for ((hOffset, vOffset) <- horizontalVerticalOffsets if fittingXY(table)(Position(x + hOffset, y + vOffset))) {
          set += Position(x + hOffset, y + vOffset).xy
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          val tuple = (abs(x - x2), abs(y - y2))
          (1, 2) == tuple || (2, 1) == tuple
      }
  }

  case object King extends Piece(4) {
    val minusOneToOne = Array(-1, 0, 1)

    override def attackPositions(x: Int, y: Int)(table: Table): Positions = {
      build({ set =>
        for (hOffset <- minusOneToOne if fittingX(table)(x + hOffset);
             vOffset <- minusOneToOne if fittingY(table)(y + vOffset)) {
          set += Position(x + hOffset, y + vOffset).xy
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) <= 1 && abs(y - y2) <= 1
      }
  }

  def fittingX(table: Table)(x: Int): Boolean = 0 <= x && x < table.horizontal

  def fittingY(table: Table)(y: Int): Boolean = 0 <= y && y < table.vertical

  def fittingXY(table: Table)(position: Position): Boolean = fittingX(table)(position.x) && fittingY(table)(position.y)

  def build(adder: mutable.BitSet => Unit): BitSet = {
    val set = mutable.BitSet.empty
    adder.apply(set)
    BitSet.fromBitMask(set.toBitMask)
  }

}
