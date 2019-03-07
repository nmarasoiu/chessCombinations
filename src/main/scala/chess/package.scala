import scala.collection.immutable.BitSet

package object chess {

  case class PiecePosition(piece: Piece, position: Position)

  type Position = Int
  type Positions = BitSet //too concrete (for performance of bitset ops - could it still be achieved with Set[Int]?, but starting with that,get the performance then go towards Set[Position] and check performance remains

  case class Input(table: Table,
                   pieces: Seq[Piece], //with duplicates (to rewrite back to Map to Int
                   positions: Positions)

  object Input {

    def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
      Input(table, toSeq(piecesCount), positionsFor(table))

    def positionsFor(table: Table): Positions = {
      val positions = for (x <- 0 until table.horizontal;
                           y <- 0 until table.vertical;
                           aggNum: Position = toPosition(x, y)) yield aggNum
      BitSet(positions: _*)
    }

    def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
      //todo this will not work for many pieces; work with Map(Piece->Int) instead
      for (piece <- piecesCount.keys.toList.sorted.toArray; _ <- 1 to piecesCount(piece)) yield piece
    }
  }

  private val hh = 32768

  case class Table(horizontal: Int, vertical: Int) {
    if (horizontal < 0 || horizontal >= hh || vertical < 0 || vertical >= hh)
      throw new IllegalArgumentException((horizontal, vertical) + " not within range: 0<=x<=" + hh + " and same for y")
  }

  case class PotentialSolution(solution: Set[PiecePosition])

  def toPosition(x: Int, y: Int): Position = x + y * hh

  def fromPosition(xy: Position): (Position, Position) = (xy % hh, xy / hh)

}
