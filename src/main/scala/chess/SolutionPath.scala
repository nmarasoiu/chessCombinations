package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}

import scala.collection.immutable.{BitSet, Map, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[PartialSolution] =
    solutions(table, pieces, positions = PositionSet(BitSet(0 until table.vertical.height * table.horizontal.length: _*)))

  def solutions(table: Table, pieces: Map[Piece, Count], positions: PositionSet): Flowable[PartialSolution] = {
    if (table.vertical.height <= 0 || table.horizontal.length <= 0) {
      empty()
    } else {
      val newRemainingPieces = for (
        (piece, pieceCount@Count(count)) <- pieces if count > 0
      ) yield (piece, (pieceCount, position0))
      SolutionPath(table).solutions(
        remainingPositions = positions,
        partialSolutionSoFar = PartialSolution.Empty,
        remainingPieces = TreeMap[Piece, (Count, Position)]() ++ newRemainingPieces,
        positionsTakenSoFar = PositionSet(BitSet()),
        firstLevel = true)
    }
  }

  val position0 = Position(0)
}

case class SolutionPath(table: Table) {

  private val empty: Flowable[PartialSolution] = Flowable.empty[PartialSolution]

  def solutions(remainingPositions: PositionSet,
                partialSolutionSoFar: PartialSolution,
                positionsTakenSoFar: PositionSet,
                remainingPieces: Map[Piece, (Count, Position)],
                firstLevel: Boolean): Flowable[PartialSolution] = {
    if (remainingPieces.isEmpty) {
      just(partialSolutionSoFar)
    } else {
      val (piece, (count, minPosition)) = remainingPieces.head

      def solutionsForPick(position: Position): Flowable[PartialSolution] = {
        val incompatiblePositions: PositionSet = piece.incompatiblePositions(PositionInTable(position, table))
        if (positionsTakenSoFar.intersects(incompatiblePositions)) {
          empty
        } else {
          solutions(
            partialSolutionSoFar = partialSolutionSoFar + Pick(piece, position),
            remainingPositions = remainingPositions - incompatiblePositions,
            positionsTakenSoFar = positionsTakenSoFar + position,
            remainingPieces = count match {
              case Count(1) => remainingPieces - piece
              case _ => remainingPieces + (piece -> (count.decremented(), position.next()))
            },
            firstLevel = false)
        }
      }

      val positionFlow: Flowable[Position] =
        fromIterable(remainingPositions.filter(pos => pos >= minPosition))
      if (firstLevel)
        positionFlow.flatMapInParallel(solutionsForPick)
      else
        positionFlow.flatMap(solutionsForPick)
    }
  }
}

