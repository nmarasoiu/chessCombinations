package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.just

import scala.collection.immutable.{Map, SortedMap, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[SubSolution] =
    solutions(table, pieces, positions = PositionSet(0 until table.vertical.height * table.horizontal.length))

  def solutions(table: Table, pieces: Map[Piece, Count], positions: PositionSet): Flowable[SubSolution] = {
    SolutionPath(table).solutions(
      partialSolutionSoFar = SubSolution(),
      positionsTakenSoFar = PositionSet(),
      remainingPositions = positions,
      remainingPieces = TreeMap[Piece, (Count, Position)]() ++ (
        for ((piece, pieceCount) <- pieces) yield (piece, (pieceCount, Position.zero))),
      firstLevel = true)
  }
}

case class SolutionPath(table: Table) {

  def solutions(remainingPositions: PositionSet,
                partialSolutionSoFar: SubSolution,
                positionsTakenSoFar: PositionSet,
                remainingPieces: SortedMap[Piece, (Count, Position)],
                firstLevel: Boolean): Flowable[SubSolution] = {

    remainingPieces.headOption match {
      case None =>
        just(partialSolutionSoFar)

      case Some((piece, (count, minPosition))) =>
        def solutionsForPick(position: Position): Flowable[SubSolution] = {
          val incompatiblePositions: PositionSet = piece.incompatiblePositions(position, table)
          if (positionsTakenSoFar.intersects(incompatiblePositions)) {
            Flowable.empty[SubSolution]
          } else {
            solutions(
              partialSolutionSoFar = partialSolutionSoFar + Pick(piece, position),
              remainingPositions = remainingPositions - incompatiblePositions,
              positionsTakenSoFar = positionsTakenSoFar + position,
              remainingPieces = count match {
                case Count.one => remainingPieces - piece
                case _ => remainingPieces + (piece -> (count.decremented(), position.next()))
              },
              firstLevel = false)
          }
        }

        fromIterable(remainingPositions.iterableFrom(minPosition))
          .flatMapScala(mapper = solutionsForPick)(inParallel = firstLevel)
    }
  }
}

