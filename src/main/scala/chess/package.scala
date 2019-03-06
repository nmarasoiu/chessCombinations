import scala.collection.immutable.BitSet

package object chess {

  case class PiecePosition(piece: Piece, position: PositionInt)

  type PositionInt = Int
  type Position = PositionInt
  type PieceInt = Int
  type Positions = BitSet //too concrete, but starting with that,get the performance then go towards Set[PositionInt] and check performance remains

  case class Input(table: Table,
                   pieces: Seq[Piece], //with duplicates
                   positions: Positions)

  object Input {

    def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
      Input(table, toStream(piecesCount), positionsFor(table))

    def positionsFor(table: Table): Positions = {
      def toSet(values: Iterable[Int]): Positions = BitSet.empty ++ values
      toSet(for (x <- 0 until table.horizontal;
                 y <- 0 until table.vertical;
                 aggNum: PositionInt = toPositionInt(x, y)) yield aggNum)
    }

    def toStream(piecesCount: Map[Piece, Int]): Stream[Piece] = {
      for (piece <- piecesCount.keys.toStream; _ <- 1 to piecesCount(piece)) yield piece
    }
  }

  private val hh = 32768

  case class Table(horizontal: Int, vertical: Int) {
    if (horizontal < 0 || horizontal >= hh || vertical < 0 || vertical >= hh)
      throw new IllegalArgumentException((horizontal, vertical) + " not within range: 0<=x<=" + hh + " and same for y")
  }

  case class PotentialSolution(solution: Set[PiecePosition])

  def toPositionInt(x: Int, y: Int): PositionInt = x + y * hh

  def fromPositionInt(xy: PositionInt): (PositionInt, PositionInt) = (xy % hh, xy / hh)

}
