package chess

import chess.Enrichments._
import io.reactivex.Flowable

import scala.collection.immutable.{Map, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[SubSolution] =
    solutions(table, pieces, positions = PositionSet(0 until table.area))

  private def solutions(table: Table, pieces: Map[Piece, Count],
                        positions: PositionSet): Flowable[SubSolution] = {
    val root = SolutionPath(table, firstLevel = true, remainingPositions = positions,
      positionsTakenSoFar = PositionSet(), partialSolutionSoFar = SubSolution(),
      remainingPieces = TreeMap[Piece, (Count, Position)]() ++ pieces.mapValues(count => (count, Position.zero)))
    root.solutions().toFlowable
  }
}

case class SolutionPath(table: Table,firstLevel: Boolean,
                        remainingPositions: PositionSet,
                        positionsTakenSoFar: PositionSet,
                        partialSolutionSoFar: SubSolution,
                        remainingPieces: Map[Piece, (Count, Position)]) {

  def solutions(): Belt[SubSolution] = {
    remainingPieces.minOption((x, y) => x.compare(y)) match {
      case None =>
        Belt(partialSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        Belt(
          iterator = remainingPositions.iteratorFrom(minPosition))(
          inParallel = firstLevel
        ).flatMap(position => {
          val incompatiblePositions = piece.incompatiblePositions(position, table)
          if (positionsTakenSoFar.intersects(incompatiblePositions)) {
            Belt()
          } else {
            SolutionPath(table, firstLevel = false,
              positionsTakenSoFar = positionsTakenSoFar + position,
              remainingPositions = remainingPositions - incompatiblePositions,
              partialSolutionSoFar = partialSolutionSoFar + (piece, position),
              remainingPieces = count match {
                case Count.one => remainingPieces - piece
                case _ => remainingPieces + (piece -> (count.decremented(), position.next()))
              }).solutions()
          }
        })
    }
  }
}

