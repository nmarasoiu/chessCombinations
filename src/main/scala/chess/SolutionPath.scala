package chess

import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable

import scala.collection.immutable.{Map, SortedMap, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[SubSolution] =
    solutionsAsParallel(table, pieces).sequential()

  def solutionsAsParallel(table: Table, pieces: Map[Piece, Count]): ParallelFlowable[SubSolution] =
    solutionsAsParallel(table, pieces, positions = PositionSet(0 until table.area))

  def solutionsAsParallel(table: Table, pieces: Map[Piece, Count],
                          positions: PositionSet): ParallelFlowable[SubSolution] = {
    SolutionPath(table)
      .solutions(firstLevel = true,
        remainingPositions = positions,
        positionsTakenSoFar = PositionSet(),
        partialSolutionSoFar = SubSolution(),
        remainingPieces = TreeMap[Piece, (Count, Position)]() ++ pieces.mapValues(count => (count, Position.zero)))
      .parallelFlowable()
  }
}

case class SolutionPath(table: Table) {

  def solutions(remainingPositions: PositionSet,
                positionsTakenSoFar: PositionSet,
                partialSolutionSoFar: SubSolution,
                remainingPieces: SortedMap[Piece, (Count, Position)],
                firstLevel: Boolean): Monad[SubSolution] = {
    remainingPieces.headOption match {
      case None =>
        SingletonMonad(partialSolutionSoFar)

      case Some((piece, (count, minPosition))) =>
        def solutionsForPick(position: Position): Monad[SubSolution] = {
          val incompatiblePositions: PositionSet = piece.incompatiblePositions(position, table)
          if (positionsTakenSoFar.intersects(incompatiblePositions)) {
            EmptyMonad()
          } else {
            solutions(firstLevel = false,
              positionsTakenSoFar = positionsTakenSoFar + position,
              remainingPositions = remainingPositions - incompatiblePositions,
              partialSolutionSoFar = partialSolutionSoFar + Pick(piece, position),
              remainingPieces = count match {
                case Count.one => remainingPieces - piece
                case _ => remainingPieces + (piece -> (count.decremented(), position.next()))
              })
          }
        }

        val positionsIterator = remainingPositions.iteratorFrom(minPosition)
        val positionsMonad =
          if (firstLevel)
            ParallelFlowableMonad(positionsIterator.toIterable)
          else
            IteratorMonad(positionsIterator)
        positionsMonad.flatMap(position => solutionsForPick(position))
    }
  }
}

