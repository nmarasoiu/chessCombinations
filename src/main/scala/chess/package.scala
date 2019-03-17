import scala.collection.immutable.{BitSet, Map, SortedMap, TreeMap}

package object chess {
  type Position = Int
  type PiecePositionInt = Int
  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = BitSet // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece
  type PieceInt = Int
  type PieceCount = Int
  val minTaskSize = 432150

  case class Input(table: Table,
                   pieces: SortedMap[Piece, PieceCount],
                   positions: Positions)

  final case class Table(horizontal: Int, vertical: Int) {
    val moduloFactor: Int = horizontal + 1

    def fromPairToInt(x: Int, y: Int): Int = x + y * moduloFactor

    def fromIntToPair(xy: Int): (Int, Int) = {
      (xy % moduloFactor, xy / moduloFactor)
    }
  }

  case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

  object Input {
    def from(table: Table, piecesToPositions: Map[Piece, Position]) =
      Input(table, toSortedPieceCount(piecesToPositions), positionsFor(table))


    //    def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
    //      Input(table, toSortedPieceCount(piecesCount), positionsFor(table))

    private def toSortedPieceCount(piecesCount: Map[Piece, PieceCount]): SortedMap[Piece, PieceCount] =
      TreeMap[Piece, PieceCount]() ++ piecesCount.map { case (piece, count) => (piece, count) }

    private def positionsFor(table: Table): Positions = {
      val positions = for (x <- 0 until table.horizontal;
                           y <- 0 until table.vertical;
                           aggNum = table.fromPairToInt(x, y)) yield aggNum
      BitSet(positions: _*)
    }
  }

  object PiecePosition {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def fromIntToPieceAndCoordinates(piecePositionInt: PiecePositionInt, table: Table): PieceAndCoordinates =
      PieceAndCoordinates(piece(piecePositionInt), table.fromIntToPair(position(piecePositionInt)))

    def piece(piecePositionInt: PiecePositionInt): Piece = Piece.of(piecePositionInt & pieceEncodingOnes)

    def position(piecePositionInt: PiecePositionInt): Int = piecePositionInt >>> pieceEncodingBits

    def toInt(piece: Piece, position: Position): Position = (position << PiecePosition.pieceEncodingBits) + piece.order
  }

  object Solution {
    def fromIntToPieceAndCoordinates(piecePositions: Solution, table: Table): Seq[PieceAndCoordinates] = {
      (for (piecePosition <- piecePositions) yield PiecePosition.fromIntToPieceAndCoordinates(piecePosition, table))
        .toIndexedSeq.sortBy(_.piece)
    }
  }

}
