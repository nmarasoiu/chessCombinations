import scala.collection.immutable.{BitSet, Map}

package object chess {
  val minTaskSize = 145

  type Position = Int
  type PiecePositionInt = Int
  type Positions = BitSet
  type PiecePositions = BitSet
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

  case class Table(horizontal: Int, vertical: Int)

  final object PiecePosition {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def fromInt(piecePositionInt: PiecePositionInt): (Piece, Position) = (piece(piecePositionInt), position(piecePositionInt))

    def toInt(piece: Piece, position: Position): Position = (position << PiecePosition.pieceEncodingBits) + piece.order

    def piece(piecePositionInt: PiecePositionInt): Piece = Piece.of(piecePositionInt & pieceEncodingOnes)

    def position(piecePositionInt: PiecePositionInt): Int = piecePositionInt >>> pieceEncodingBits
  }

  final object Position {

    def fromPairToInt(x: Int, y: Int, table: Table): Int = x + y * horizontal(table)

    def fromIntToPair(xy: Int, table: Table): (Int, Int) = (xy % horizontal(table), xy / horizontal(table))

    def horizontal(table: Table): Int = {
      table.horizontal + 1
    }
  }

  case class PotentialSolution(solution: PiecePositions)

}
