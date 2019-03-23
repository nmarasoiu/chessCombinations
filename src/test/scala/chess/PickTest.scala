package chess

import chess.Pick.{piece, position}

case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))
object PickTest {

  def fromIntToPieceAndCoordinates(piecePositionInt: Pick, table: Table): PieceAndCoordinates = {
    val pos = position(piecePositionInt)
    val x = table.x(pos)
    val y = table.y(pos)

    PieceAndCoordinates(piece(piecePositionInt), (x.x,y.y))
  }

}
