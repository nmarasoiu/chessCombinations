package chess

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed abstract class Piece(val order: Int,
                            protected val attackPositions: Function[(Position, Table), Seq[Position]], //currying?
                           ) extends EnumEntry with Ordered[Piece] {
  lazy val incompatPositions: Function[(Position, Table), Set[Position]] = {
    case (pos, table@Table(h, v)) =>
      val incompatiblePositions = attackPositions(pos, table).toSet
      for (Position(x, y) <- incompatiblePositions if 0 <= x && x < h && 0 <= y && y <= v)
        yield Position(x, y)
  }

  def compare(that: Piece): Int = this.order - that.order
}

object Piece extends Enum[Piece] {
  lazy val values: immutable.IndexedSeq[Piece] = findValues

  case object King extends Piece(2, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- Seq(-1, 0, 1) if x + hOffset >= 0;
           vOffset <- Seq(-1, 0, 1) if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })

  case object Queen extends Piece(3,
    a => Rook.attackPositions(a) ++ Bishop.attackPositions(a))


  //nebun
  case object Bishop extends Piece(4, {
    case (Position(x, y), Table(h, v)) => //todo optimize
      for (hOffset <- -h until h) yield Position(x + hOffset, y + hOffset)
  })

  //cal
  case object Knight extends Piece(5, {
    case (Position(x, y), Table(h, v)) =>
      for ((absHorizOffset, absVertOffset) <- Seq((1, 2), (2, 1));
           (hOffset, vOffset) <- Seq(
             (absHorizOffset, absVertOffset), (-absHorizOffset, absVertOffset),
             (absHorizOffset, -absVertOffset), (-absHorizOffset, -absVertOffset)))
        yield Position(x + hOffset, y + vOffset)
  })

  //tura
  case object Rook extends Piece(6, {
    case (Position(x, y), Table(h, v)) =>
      (for (hOffset <- 0 until h) yield Position(hOffset, y)) ++
        (for (vOffset <- 0 until v) yield Position(x, vOffset))
  })

}
