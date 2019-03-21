import scala.collection.immutable.BitSet

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

  case class Table(horizontal: Int, vertical: Int) {
    override lazy val hashCode: Int = (horizontal, vertical).hashCode * 31

    def fromPairToInt(x: Int, y: Int): Int = x + y * horizontal

    def fromIntToPair(xy: Int): (Int, Int) = (xy % horizontal, xy / horizontal)
  }

  case class PositionInTable(table: Table, position: Position) {
    override lazy val hashCode: PieceCount = table.hashCode + position
  }

  case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

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
