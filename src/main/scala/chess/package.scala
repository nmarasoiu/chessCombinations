import java.util

import scala.collection.immutable.{BitSet, Map, TreeMap}

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

  final case class Table(horizontal: Int, vertical: Int) {
    override lazy val hashCode: Int = horizontal * 31 + vertical

    def fromPairToInt(x: Int, y: Int): Int = x + y * horizontal

    def fromIntToPair(xy: Int): (Int, Int) = (xy % horizontal, xy / horizontal)

    override def equals(obj: Any): Boolean = Utils.equals(obj, (other: Table) => isEqualTo(other), classOf[Table])

    private def isEqualTo(other: Table): Boolean = horizontal == other.horizontal && vertical == other.vertical
  }

  final case class PositionInTable(position: Position, table: Table) {
    override lazy val hashCode: Int = position.hashCode * 31 + table.hashCode

    override def equals(obj: Any): Boolean = Utils.equals(obj, (other: PositionInTable) => isEqualTo(other), classOf[PositionInTable])

    private def isEqualTo(other: PositionInTable): Boolean = position == other.position && table == other.table
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
