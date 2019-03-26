package chess

case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

object PickTest {

  def fromIntToPieceAndCoordinates(pick: Pick, table: Table): PieceAndCoordinates = {
    val pos = pick.position
    val x = pos.x(table)
    val y = pos.y(table)
    PieceAndCoordinates(pick.piece, (x.value, y.value))
  }

}
