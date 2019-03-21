package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}

import scala.collection.immutable.{BitSet, Map}

object SolutionPath {

  def solutions(table: Table,
                pieces: Map[Piece, PieceCount],
                positions: Positions): Flowable[Solution] = {
    if (table.vertical <= 0 || table.horizontal <= 0) {
      empty()
    } else {
      SolutionPath(table).solutions(
        remainingPositions = positions,
        builtSolutionSoFar = EmptyList$PiecePosition,
        remainingPieces = pieces.mapValues(count => (count, 0)),
        positionsTakenSoFar = BitSet(),
        firstLevel = true)
    }
  }
}

case class SolutionPath(table: Table) {

  private val empty: Flowable[Solution] = Flowable.empty[Solution]

  def solutions(remainingPositions: Positions,
                builtSolutionSoFar: Solution,
                positionsTakenSoFar: Positions,
                remainingPieces: Map[Piece, (PieceCount, Position)],
                firstLevel: Boolean): Flowable[Solution] = {
    if (remainingPieces.isEmpty) {
      just(builtSolutionSoFar)
    } else {
      val (piece, (pieceCount, minPosition)) = remainingPieces.min
      val positionFlow = fromIterable(remainingPositions.filter(pos => pos >= minPosition))
      positionFlow.flatMap1(inParallel = firstLevel) {
        position => {
          val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
          if ((positionsTakenSoFar & incompatiblePositions ).nonEmpty) {
            empty
          } else {
            solutions(
              remainingPieces = if (pieceCount == 1) remainingPieces - piece else remainingPieces + (piece -> (pieceCount - 1, position + 1)),
              builtSolutionSoFar = PickListCons(PiecePosition.toInt(piece, position), builtSolutionSoFar),
              remainingPositions = remainingPositions &~ incompatiblePositions,
              positionsTakenSoFar = positionsTakenSoFar + position,
              firstLevel = false)
          }
        }
      }
    }
  }
}

