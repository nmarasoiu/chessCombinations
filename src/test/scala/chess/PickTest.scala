package chess

import chess.BlockingUtil.Sol
import chess.model.{Piece, _}

case class PieceAndCoordinates(piece: Piece, coordinates: (Int, Int))

object PickTest {

  private def print[T](table: Table, solution: Sol): Unit = {
    println(
      (for (pick <- solution.picks)
        yield fromIntToPieceAndCoordinates(pick, table)
        ).toIndexedSeq.sortBy(_.piece))
  }

  def fromIntToPieceAndCoordinates(pick: Pick, table: Table): PieceAndCoordinates = {
    val pos = pick.position
    val x = pos.x(table)
    val y = pos.y(table)
    PieceAndCoordinates(pick.piece, (x.value, y.value))
  }

}
