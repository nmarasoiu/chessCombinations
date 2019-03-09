import scala.collection.immutable.{BitSet, Map}

package object chess {

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
                           aggNum = Position(x, y,table).xy) yield aggNum
      BitSet(positions: _*)
    }
  }

  case class Table(horizontal: Int, vertical: Int) {
  }
  case class PiecePosition(piece: Piece, position: Position) {
    override def toString: String = (piece, position).toString
  }

  final case class Position(xy:Int,table:Table){
    val hh: Int = Position.horizontal(table)
    def x: Int = xy % hh
    def y: Int = xy / hh
    def pair: (Int, Int) = (x, y)
  }
  object Position {
    def apply(x: Int, y: Int, table:Table): Position = Position(x + y * horizontal(table), table)

    def zero(table:Table): Position = Position(0, table)

    def horizontal(table:Table): Int ={
      table.horizontal+1
    }

  }

  case class PotentialSolution(solution: Set[PiecePosition])


}
