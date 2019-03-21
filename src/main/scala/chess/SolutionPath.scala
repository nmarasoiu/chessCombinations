package chess

import chess.FlowableUtils._
import chess.Utils._
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
        builtSolutionSoFar = Empty,
        remainingPieces = pieces.filter(_._2 > 0).mapValues(count => (count, 0)),
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
    remainingPieces.minOption() match {
      case None =>
        just(builtSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        val positionFlow = fromIterable(remainingPositions.filter(pos => pos >= minPosition))
        positionFlow.flatMap1(inParallel = firstLevel) {
          position => {
            val incompatiblePositions = piece.incompatiblePositions(PositionInTable(table, position))
            if (positionsTakenSoFar.intersects(incompatiblePositions)) {
              empty
            } else {
              val newRemainingPieces = count match {
                case 1 => remainingPieces - piece
                case _ => remainingPieces + (piece -> (count - 1, position + 1))
              }
              solutions(
                remainingPieces = newRemainingPieces,
                builtSolutionSoFar = Cons(PiecePosition.toInt(piece, position), builtSolutionSoFar),
                remainingPositions = remainingPositions &~ incompatiblePositions,
                positionsTakenSoFar = positionsTakenSoFar + position,
                firstLevel = false)
            }
          }
        }
    }
  }
}

