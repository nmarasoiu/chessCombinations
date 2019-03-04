package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed abstract class Piece(val order: Int,
                            protected val attackPositions: Function[(Position, Table), Seq[Position]],
                           ) extends EnumEntry with Ordered[Piece] {
  lazy val incompatiblePositions: Function[(Position, Table), Set[Position]] = {
    case (pos, table@Table(h, v)) =>
      attackPositions(pos, table).filter {
        case Position(x, y) => 0 <= x && x < h && 0 <= y && y <= v
      }.toSet
  }

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  lazy val values: immutable.IndexedSeq[Piece] = findValues

  case object King extends Piece(2, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- -1 to 1 if x + hOffset >= 0;
           vOffset <- -1 to 1 if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })

  case object Queen extends Piece(3,
    a => Rook.attackPositions(a) ++ Bishop.attackPositions(a))


  //nebun
  case object Bishop extends Piece(4, {
    case (Position(x, y), Table(h, v)) => //todo optimize
      for (hOffset <- 1 - h until h) yield Position(x + hOffset, y + hOffset)
  })

  //cal
  case object Knight extends Piece(5, {
    case (Position(x, y), Table(h, v)) =>
      for ((absHorizontalOffset, absVerticalOffset) <- Seq((1, 2), (2, 1));
           (hOffset, vOffset) <- Seq(
             (absHorizontalOffset, absVerticalOffset), (-absHorizontalOffset, absVerticalOffset),
             (absHorizontalOffset, -absVerticalOffset), (-absHorizontalOffset, -absVerticalOffset)))
        yield Position(x + hOffset, y + vOffset)
  })

  //tura
  case object Rook extends Piece(6, {
    case (Position(x, y), Table(h, v)) =>
      Seq(
        for (hOffset <- 0 until h) yield Position(hOffset, y),
        for (vOffset <- 0 until v) yield Position(x, vOffset)).flatten
  })

}
