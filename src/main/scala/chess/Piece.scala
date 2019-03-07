package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable.BitSet
import scala.collection.{immutable, mutable}
import scala.math.abs

sealed abstract class Piece(val order: Int) extends EnumEntry with Ordered[Piece] {
  def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean

  def attackPositions(position: Position, table: Table): Positions

  def takes(piecePosition: Position, otherPosition: Position): Boolean = {
    val xy1 = fromPosition(piecePosition)
    val xy2 = fromPosition(otherPosition)
    takes(xy1, xy2)
  }

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object Queen extends Piece(0) {
    override def attackPositions(a: Position, table: Table): Positions = {
      Rook.attackPositions(a, table) | Bishop.attackPositions(a, table)
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      Rook.takes(piecePosition, otherPosition) || Bishop.takes(piecePosition, otherPosition)
  }

  //nebun
  case object Bishop extends Piece(1) {
    override def attackPositions(xy: Position, table: Table): Positions = {
      val (x, y) = fromPosition(xy)
      build({ set =>
        val h = table.horizontal
        for (hOffset <- 1 - h until h if 0 <= x + hOffset && 0 <= y + hOffset) {
          set += toPosition(x + hOffset, y + hOffset)
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) == abs(y - y2)
      }
  }

  //tura
  case object Rook extends Piece(2) {
    override def attackPositions(xy: Position, table: Table): Positions = {
      val (x, y) = fromPosition(xy)
      build({ set =>
        for (hOffset <- 0 until table.horizontal) {
          set += toPosition(hOffset, y)
        }
        for (vOffset <- 0 until table.vertical) {
          set += toPosition(x, vOffset)
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) => x == x2 || y == y2
      }
  }

  //cal
  case object Knight extends Piece(3) {
    val horizontalVerticalOffsets: Array[(Int, Int)] =
      for ((absHorizontalOffset, absVerticalOffset) <- Array((1, 2), (2, 1));
           (hOffset, vOffset) <- Seq(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield (hOffset, vOffset)

    override def attackPositions(xy: Position, table: Table): Positions = {
      val (x, y) = fromPosition(xy)
      build({ set =>
        for ((hOffset, vOffset) <- horizontalVerticalOffsets if x + hOffset >= 0 && 0 <= y + vOffset) {
          set += toPosition(x + hOffset, y + vOffset)
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
    override def attackPositions(xy: Position, table: Table): Positions = {
      val (x, y) = fromPosition(xy)
      build({ set =>
        for (hOffset <- -1 to 1 if x + hOffset >= 0;
             vOffset <- -1 to 1 if y + vOffset >= 0) {
          set += toPosition(x + hOffset, y + vOffset)
        }
      })
    }

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) <= 1 && abs(y - y2) <= 1
      }
  }

  def build(adder: mutable.BitSet => Unit): BitSet = {
    val set = mutable.BitSet.empty
    adder.apply(set)
    BitSet.fromBitMask(set.toBitMask)
  }

}
