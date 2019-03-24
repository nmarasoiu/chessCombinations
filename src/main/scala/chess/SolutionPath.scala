package chess

import io.reactivex.Flowable

import scala.collection.immutable.{Map, SortedMap, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[SubSolution] =
    solutions(table, pieces, positions = PositionSet(0 until table.area))

  private def solutions(table: Table, pieces: Map[Piece, Count],
                positions: PositionSet): Flowable[SubSolution] = {
    SolutionPath(table)
      .solutions(firstLevel = true,
        remainingPositions = positions,
        positionsTakenSoFar = PositionSet(),
        partialSolutionSoFar = SubSolution(),
        remainingPieces = TreeMap[Piece, (Count, Position)]() ++ pieces.mapValues(count => (count, Position.zero)))
      .flowable()
  }
}

case class SolutionPath(table: Table) {

  def solutions(remainingPositions: PositionSet,
                positionsTakenSoFar: PositionSet,
                partialSolutionSoFar: SubSolution,
                remainingPieces: SortedMap[Piece, (Count, Position)],
                firstLevel: Boolean): Belt[SubSolution] = {

    def solutionsForPick(position: Position, piece: Piece, count: Count): Belt[SubSolution] = {
      val incompatiblePositions = piece.incompatiblePositions(position, table)
      if (positionsTakenSoFar.intersects(incompatiblePositions)) {
        Belt()
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

    remainingPieces.headOption match {
      case None =>
        Belt(partialSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        Belt(
          iterator = remainingPositions.iteratorFrom(minPosition))(
          inParallel = firstLevel
        ).flatMap(
          position => solutionsForPick(position, piece, count))
    }
  }
}

