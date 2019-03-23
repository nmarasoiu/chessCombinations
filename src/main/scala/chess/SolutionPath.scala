package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}

import scala.collection.immutable.{BitSet, Map, TreeMap}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, Count]): Flowable[Solution] =
    solutions(table, pieces, positions = BitSet(0 until table.vertical.height * table.horizontal.length: _*))

  def solutions(table: Table, pieces: Map[Piece, Count], positions: Positions): Flowable[Solution] = {
    if (table.vertical.height <= 0 || table.horizontal.length <= 0) {
      empty()
    } else {
      val newRemainingPieces = for (
        (piece, pieceCount@Count(count)) <- pieces if count > 0
      ) yield (piece, (pieceCount, position0))
      SolutionPath(table).solutions(
        remainingPositions = positions,
        builtSolutionSoFar = Nil,
        remainingPieces = TreeMap[Piece, (Count, Position)]() ++ newRemainingPieces,
        positionsTakenSoFar = BitSet(),
        firstLevel = true)
    }
  }

  val position0 = Position(0)
}

case class SolutionPath(table: Table) {

  private val empty: Flowable[Solution] = Flowable.empty[Solution]

  def solutions(remainingPositions: Positions,
                builtSolutionSoFar: Solution,
                positionsTakenSoFar: Positions,
                remainingPieces: Map[Piece, (Count, Position)],
                firstLevel: Boolean): Flowable[Solution] = {
    if (remainingPieces.isEmpty) {
      just(builtSolutionSoFar)
    } else {
      val (piece, (Count(count), Position(minPosition))) = remainingPieces.head

      def solutionsForPick(position: Position): Flowable[Solution] = {
        val Position(positionInt) = position
        val positionInTable = PositionInTable(position, table)
        val incompatiblePositions = piece.incompatiblePositions(positionInTable)
        if (positionsTakenSoFar.intersects(incompatiblePositions)) {
          empty
        } else {
          val newRemainingPieces = count match {
            case 1 => remainingPieces - piece
            case _ => remainingPieces + (piece -> (Count(count - 1), Position(positionInt + 1)))
          }
          solutions(
            remainingPieces = newRemainingPieces,
            builtSolutionSoFar = ::(Pick(piece, position), builtSolutionSoFar),
            remainingPositions = remainingPositions &~ incompatiblePositions,
            positionsTakenSoFar = positionsTakenSoFar + positionInt,
            firstLevel = false)
        }
      }

      val positionFlow: Flowable[Position] =
        fromIterable(remainingPositions.filter(pos => pos >= minPosition).map(pos => Position(pos)))
      if (firstLevel)
        positionFlow.flatMapInParallel(solutionsForPick)
      else
        positionFlow.flatMap(solutionsForPick)
    }
  }
}

