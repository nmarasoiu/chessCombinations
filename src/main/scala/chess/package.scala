import scala.collection.immutable.{BitSet, Map}

package object chess {
  type Position = Int
  type PiecePositionInt = Int
  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = BitSet // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece
  type OrderedPiecesWithCount = Map[Piece, Int]
  val minTaskSize = 145

  case class Input(table: Table,
                   pieces: OrderedPiecesWithCount,
                   positions: Positions)

  case class Table(horizontal: Int, vertical: Int)

  case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

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

  final object PiecePosition {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def fromIntToPieceAndCoordinates(piecePositionInt: PiecePositionInt, table: Table): PieceAndCoordinates =
      PieceAndCoordinates(piece(piecePositionInt), Position.fromIntToPair(position(piecePositionInt), table))

    def piece(piecePositionInt: PiecePositionInt): Piece = Piece.of(piecePositionInt & pieceEncodingOnes)

    def position(piecePositionInt: PiecePositionInt): Int = piecePositionInt >>> pieceEncodingBits

    def toInt(piece: Piece, position: Position): Position = (position << PiecePosition.pieceEncodingBits) + piece.order
  }

  final object Position {

    def fromPairToInt(x: Int, y: Int, table: Table): Int = x + y * horizontal(table)

    def horizontal(table: Table): Int = {
      table.horizontal + 1
    }

    def fromIntToPair(xy: Int, table: Table): (Int, Int) = (xy % horizontal(table), xy / horizontal(table))
  }

  object Solution {
    def fromIntToPieceAndCoordinates(piecePositions: Solution, table: Table): Seq[PieceAndCoordinates] = {
      (for (piecePosition <- piecePositions) yield PiecePosition.fromIntToPieceAndCoordinates(piecePosition, table))
        .toSeq.sortBy(_.piece)
    }
  }

}
