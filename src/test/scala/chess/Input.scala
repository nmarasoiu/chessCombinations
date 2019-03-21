package chess

import scala.collection.immutable.{BitSet, Map}

case class Input(table: Table,
                 pieces: Map[Piece, PieceCount],
                 positions: Positions)

object Input {
  def from(table: Table, pieces: Map[Piece, PieceCount]) =
    Input(table, pieces, positions = BitSet(0 until table.vertical * table.horizontal: _*))
}