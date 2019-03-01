package chess

import enumeratum.Enum
import enumeratum.EnumEntry
import math.abs

sealed abstract class ChessPiece(val order: Int,
                                 val eval: Function[(Position, Position), Boolean],
                                 val attackPositions: Function[(Position, Table), Seq[Position]],
                                ) extends EnumEntry with Ordered[ChessPiece] {
  def compare(that: ChessPiece): Int = this.order - that.order
}

object ChessPiece extends Enum[ChessPiece] {
  val values = findValues

  case object King extends ChessPiece(2, {
    case (Position(x, y), Position(a, b)) => abs(x - a) <= 1 && abs(y - b) <= 1
  }, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- List(-1, 0, 1) if x + hOffset >= 0;
           vOffset <- List(-1, 0, 1) if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })

  case object Queen extends ChessPiece(3, pair => Bishop.eval(pair) || Knight.eval(pair), {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- List(-1, 0, 1) if x + hOffset >= 0;
           vOffset <- List(-1, 0, 1) if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })


  //nebun
  case object Bishop extends ChessPiece(4, {
    case (Position(x, y), Position(a, b)) => abs(x - a) == abs(y - b)
  }, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- 0 until h)
        yield Position(hOffset, y /*+ vOffset*/)
  })

  //cal
  case object Knight extends ChessPiece(5, {
    case (Position(x, y), Position(a, b)) => x == a || y == b
  }, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- List(-1, 0, 1) if x + hOffset >= 0;
           vOffset <- List(-1, 0, 1) if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })

  //tura
  case object Rook extends ChessPiece(6, {
    case (Position(x, y), Position(a, b)) => x == a || y == b
  }, {
    case (Position(x, y), Table(h, v)) =>
      for (hOffset <- List(-1, 0, 1) if x + hOffset >= 0;
           vOffset <- List(-1, 0, 1) if y + vOffset >= 0)
        yield Position(x + hOffset, y + vOffset)
  })



}

