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
                           aggNum = Position(x, y).xy) yield aggNum
      BitSet(positions: _*)
    }
  }

  private val hh = 32768 //todo find a name

  case class Table(horizontal: Int, vertical: Int) {
    if (horizontal < 0 || horizontal >= hh || vertical < 0 || vertical >= hh)
      throw new IllegalArgumentException((horizontal, vertical) + " not within range: 0<=x<=" + hh + " and same for y")
  }
  case class PiecePosition(piece: Piece, position: Position) {
    override def toString: String = (piece, position).toString
  }

  case class Position(xy:Int){
    def x: Int = xy % hh
    def y: Int = xy / hh
    def pair: (Int, Int) = (x, y)
  }
  object Position {
    def apply(x: Int, y: Int): Position = Position(x + y * hh)

    val zero: Position = Position(0)

  }

  case class PotentialSolution(solution: Set[PiecePosition])


}
