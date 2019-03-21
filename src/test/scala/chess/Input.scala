package chess

import scala.collection.immutable.{BitSet, Map, TreeMap}

case class Input(
                  table: Table,
                  pieces: Map[Piece, PieceCount],
                  positions: Positions
                )

object Input {
  def from(table: Table, piecesToPositions: Map[Piece, Position]) =
    Input(table, toSortedPieceCount(piecesToPositions), positionsFor(table))

  private def toSortedPieceCount(piecesCount: Map[Piece, PieceCount]): Map[Piece, PieceCount] =
    TreeMap[Piece, PieceCount]() ++ piecesCount.map { case (piece, count) => (piece, count) }

  private def positionsFor(table: Table): Positions =
    BitSet(0 until table.vertical * table.horizontal: _*)
}