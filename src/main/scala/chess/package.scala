import scala.collection.immutable.{BitSet, Map}

package object chess {
  type Position = Int
  type Positions = BitSet
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
  }

  final object Position {
    def fromPairToInt(x: Int, y: Int, table: Table): Int = x + y * horizontal(table)

    def fromIntToPair(xy: Int, table: Table): (Int, Int) = (xy % horizontal(table), xy / horizontal(table))

    def horizontal(table: Table): Int = {
      table.horizontal + 1
    }

  }

  case class PotentialSolution(solution: List[PiecePosition])


}
