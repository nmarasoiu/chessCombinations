import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.{Map, SortedMap, TreeMap}

package object chess {
  type Position = Int
  type PiecePositionInt = Int
  type Positions = RoaringBitmap //encoding (x,y) as x*horiz+y as Int
  type Solution = IntList // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece
  type PieceInt = Int
  type PieceCount = Int

  case class Input(table: Table,
                   pieces: SortedMap[Piece, PieceCount],
                   positions: Positions)

  case class Table(horizontal: Int, vertical: Int) {
    def fromPairToInt(x: Int, y: Int): Int = x + y * horizontal

    def fromIntToPair(xy: Int): (Int, Int) = {
      (xy % horizontal, xy / horizontal)
    }
  }

  case class PositionInTable(position: Position, table: Table) {
    override lazy val hashCode: Int = position
  }

  case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

  object Input {
    def from(table: Table, piecesToPositions: Map[Piece, Position]) =
      Input(table, toSortedPieceCount(piecesToPositions), positionsFor(table))

    private def toSortedPieceCount(piecesCount: Map[Piece, PieceCount]): SortedMap[Piece, PieceCount] =
      TreeMap[Piece, PieceCount]() ++ piecesCount.map { case (piece, count) => (piece, count) }

    private def positionsFor(table: Table): Positions = {
      RoaringBitmap.add(new RoaringBitmap(), 0L, table.vertical * table.horizontal)
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

}
