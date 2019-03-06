package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.{immutable, mutable}
import scala.math.abs

sealed abstract class Piece(val order: Int,
                            protected val attackPositions: Function[(Position, Table), Positions],
                           ) extends EnumEntry with Ordered[Piece] {
  def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean

  def takes(piecePosition: Position, otherPosition: Position): Boolean = {
    val xy1 = fromPositionInt(piecePosition)
    val xy2 = fromPositionInt(otherPosition)
    takes(xy1, xy2)
  }

  val incompatiblePositions: Function[(Position, Table), Positions] = {
    case (pos, table@Table(h, v)) =>
      attackPositions(pos, table)/*.filter {
        xy => {
          val (x, y) = fromPositionInt(xy)
          0 <= x && x < h && 0 <= y && y <= v
        }
      }*/
  }

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  val values: immutable.IndexedSeq[Piece] = findValues

  case object King extends Piece(1, {
    case (xy, Table(h, v)) =>
      val (x, y) = fromPositionInt(xy)
      build({ set =>
        for (hOffset <- -1 to 1 if x + hOffset >= 0;
             vOffset <- -1 to 1 if y + vOffset >= 0){
          set += toPositionInt(x + hOffset, y + vOffset)
        }
      })
  }) {
    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) <= 1 && abs(y - y2) <= 1
      }
  }

  case object Queen extends Piece(2,
    a => Rook.attackPositions(a) ++ Bishop.attackPositions(a)) {
    override val incompatiblePositions: Function[(Position, Table), Positions] = attackPositions

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      Rook.takes(piecePosition, otherPosition) || Bishop.takes(piecePosition, otherPosition)
  }

  //nebun
  case object Bishop extends Piece(3, {
    case (xy, Table(h, v)) =>
      val (x, y) = fromPositionInt(xy)
      build({ set =>
        for (hOffset <- 1 - h until h if 0 <= x + hOffset && 0 <= y + hOffset){
          set += toPositionInt(x + hOffset, y + hOffset)
        }
      })
  }) {
    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          abs(x - x2) == abs(y - y2)
      }
  }

  val horizontalVerticalOffsets: Array[(Int, Int)] =
    for ((absHorizontalOffset, absVerticalOffset) <- Array((1, 2), (2, 1));
         (hOffset, vOffset) <- Seq(
           (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
           (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
      yield (hOffset, vOffset)

  //cal
  case object Knight extends Piece(0, {
    case (xy, Table(h, v)) =>
      val (x, y) = fromPositionInt(xy)
      build({ set =>
        for ((hOffset, vOffset) <- horizontalVerticalOffsets if x + hOffset >= 0 && 0 <= y + vOffset) {
          set += toPositionInt(x + hOffset, y + vOffset)
        }
      })
  }) {
    override val incompatiblePositions: Function[(Position, Table), Positions] = attackPositions

    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) =>
          val tuple = (abs(x - x2), abs(y - y2))
          (1, 2) == tuple || (2, 1) == tuple
      }
  }

  //tura
  case object Rook extends Piece(4, {
    case (xy, Table(h, v)) =>
      val (x, y) = fromPositionInt(xy)
      build({ set =>
        for (hOffset <- 0 until h) {
          set += toPositionInt(hOffset, y)
        }
        for (vOffset <- 0 until v) {
          set += toPositionInt(x, vOffset)
        }
      })
  }) {
    override def takes(piecePosition: (Int, Int), otherPosition: (Int, Int)): Boolean =
      (piecePosition, otherPosition) match {
        case ((x, y), (x2, y2)) => x == x2 || y == y2
      }
  }

  def build(adder: mutable.BitSet => Unit): immutable.BitSet = {
    val set = mutable.BitSet.empty
    adder.apply(set)
    set.toImmutable //todo check this out
  }


}
