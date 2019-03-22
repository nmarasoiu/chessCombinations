package chess

import chess.Pick.{piece, position}

case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))
object PickTest {

  def fromIntToPieceAndCoordinates(piecePositionInt: Pick, table: Table): PieceAndCoordinates = {
    val (x,y) = table.fromIntToPair(position(piecePositionInt))
    PieceAndCoordinates(piece(piecePositionInt), (x.x,y.y))
  }

}
