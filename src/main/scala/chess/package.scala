import scala.collection.immutable.{BitSet, Map}

package object chess {
  type Position = Int
  type Positions = BitSet //todo this is too concrete? (for performance of bitset ops - could it still be achieved with Set[Int]?, but starting with that,get the performance then go towards Set[Position] and check performance remains
  type OrderedPiecesWithCount = Map[Piece, Int]

  case class Input(table: Table,
                   pieces: OrderedPiecesWithCount,
                   positions: Positions)

  object Input {

    def apply(table: Table, piecesCount: OrderedPiecesWithCount): Input =
      Input(table, piecesCount, positionsFor(table))

    def positionsFor(table: Table): Positions = {
      val positions = for (x <- 0 until table.horizontal;
                           y <- 0 until table.vertical;
                           aggNum = Position.fromPairToInt(x, y, table)) yield aggNum
      BitSet(positions: _*)
    }
  }

  case class Table(horizontal: Int, vertical: Int) {
  }

  case class PiecePosition(piece: Piece, position: Position) {
    override def toString: String = (piece, position).toString
  }

  object Position {
    def fromPairToInt(x: Int, y: Int, table: Table): Int = x + y * horizontal(table)

    def fromPairToX(xy: Int, table: Table): Int = xy % horizontal(table)

    def fromPairToY(xy: Int, table: Table): Int = xy / horizontal(table)

    def fromIntToPair(xy: Int, table: Table): (Int, Int) = (fromPairToX(xy, table), fromPairToY(xy, table))

    final def horizontal(table: Table): Int = {
      table.horizontal + 1
    }

  }

  case class PotentialSolution(solution: Set[PiecePosition])


}
