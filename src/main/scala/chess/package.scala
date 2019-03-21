import scala.collection.immutable.{BitSet, Map, SortedMap, TreeMap}

package object chess {
  type Position = Int
  type PieceInt = Int
  type PieceCount = Int
  type Pick = Int // a Piece (3bits) in a Position
  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = PickList // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece

  object Config {
    val bufferSize: Int = 64
    val printEvery: Int = 5000000
  }

  case class Input(table: Table,
                   pieces: SortedMap[Piece, PieceCount],
                   positions: Positions)

  case class Table(horizontal: Int, vertical: Int) {
    override lazy val hashCode: Int = super.hashCode

    def fromPairToInt(x: Int, y: Int): Int = x + y * horizontal

    def fromIntToPair(xy: Int): (Int, Int) =
      (xy % horizontal, xy / horizontal)

  }

  case class PositionInTable(position: Position, table: Table) {
    override lazy val hashCode: Int = position.hashCode * 31 + table.hashCode
  }

  case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

  object Input {
    def from(table: Table, piecesToPositions: Map[Piece, Position]) =
      Input(table, toSortedPieceCount(piecesToPositions), positionsFor(table))

    private def toSortedPieceCount(piecesCount: Map[Piece, PieceCount]): SortedMap[Piece, PieceCount] =
      TreeMap[Piece, PieceCount]() ++ piecesCount.map { case (piece, count) => (piece, count) }

    private def positionsFor(table: Table): Positions =
      BitSet(0 until table.vertical * table.horizontal: _*)
  }

  object PiecePosition {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def fromIntToPieceAndCoordinates(piecePositionInt: Pick, table: Table): PieceAndCoordinates =
      PieceAndCoordinates(piece(piecePositionInt), table.fromIntToPair(position(piecePositionInt)))

    def piece(piecePositionInt: Pick): Piece = Piece.of(piecePositionInt & pieceEncodingOnes)

    def position(piecePositionInt: Pick): Int = piecePositionInt >>> pieceEncodingBits

    def toInt(piece: Piece, position: Position): Position = (position << PiecePosition.pieceEncodingBits) + piece.order
  }

}
