package chess

import chess.Enrichments._
import io.reactivex.Flowable

import scala.collection.immutable.{Map, SortedMap}

object SolutionPath {
  def solutions(table: Table,
                pieces: Map[Piece, Count]): Flowable[SubSolution] =
    solutions(table, pieces, positions = PositionSet(0 until table.area))

  private def solutions(table: Table,
                        pieces: Map[Piece, Count],
                        positions: PositionSet): Flowable[SubSolution] = {
    val root = SolutionPath(table,
      firstLevel = true,
      remainingPositions = positions,
      positionsTakenSoFar = PositionSet(),
      partialSolutionSoFar = SubSolution(),
      remainingPieces = pieces.toSortedMap.mapValues(count => (count, Position.zero)))

    root.solutions().toFlowable
  }
}

case class SolutionPath(table: Table, firstLevel: Boolean,
                        remainingPositions: PositionSet,
                        positionsTakenSoFar: PositionSet,
                        partialSolutionSoFar: SubSolution,
                        remainingPieces: SortedMap[Piece, (Count, Position)]) {

  def solutions(): ConveyorBelt[SubSolution] = {
    remainingPieces.minOption() match {
      case None =>
        ConveyorBelt(partialSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        ConveyorBelt(
          iterable = remainingPositions.iterableFrom(minPosition),
          inParallel = firstLevel
        ).flatMap(position => {
          val incompatiblePositions = piece.incompatiblePositions(position, table)
          if (positionsTakenSoFar.intersects(incompatiblePositions)) {
            ConveyorBelt()
          } else {
            val nextStep = SolutionPath(table, firstLevel = false,
              positionsTakenSoFar = positionsTakenSoFar + position,
              remainingPositions = remainingPositions - incompatiblePositions,
              partialSolutionSoFar = partialSolutionSoFar + (piece, position),
              remainingPieces = count.value match {
                case 1 => remainingPieces - piece
                case _ => remainingPieces + (piece -> (count.decremented(), position.next()))
              })
            nextStep.solutions()
          }
        })
    }
  }
}

