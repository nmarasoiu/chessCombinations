package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}

import scala.collection.immutable.{BitSet, Map}

object SolutionPath {
  def solutions(table: Table, pieces: Map[Piece, PieceCount]): Flowable[Solution] =
    solutions(table, pieces, positions = BitSet(0 until table.vertical * table.horizontal: _*))

  def solutions(table: Table, pieces: Map[Piece, PieceCount], positions: Positions): Flowable[Solution] = {
    if (table.vertical <= 0 || table.horizontal <= 0) {
      empty()
    } else {
      val newRemainingPieces = pieces.filter {
        case (piece, count) => count.count > 0
      }.map {
        case (piece, count) => (piece.order, (count, Position(0)))
      }
      SolutionPath(table).solutions(
        remainingPositions = positions,
        builtSolutionSoFar = Empty,
        remainingPieces = newRemainingPieces,
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
                remainingPieces: Map[PieceId, (PieceCount, Position)],
                firstLevel: Boolean): Flowable[Solution] = {
    if (remainingPieces.isEmpty) {
      just(builtSolutionSoFar)
    } else {
      val (pieceId: PieceId, (count: PieceCount, minPosition: Position)) = remainingPieces.minBy(count => count._1.pieceInt)

      def solutionsForPick(position: Position): Flowable[Solution] = {
        val positionInTable = PositionInTable(table, position)
        val incompatiblePositions = Piece.of(pieceId).incompatiblePositions(positionInTable)
        if (positionsTakenSoFar.intersects(incompatiblePositions)) {
          empty
        } else {
          val newRemainingPieces = count.count match {
            case 1 => remainingPieces - pieceId
            case _ => remainingPieces + (pieceId -> (PieceCount(count.count - 1), Position(position.pos + 1)))
          }
          solutions(
            remainingPieces = newRemainingPieces,
            builtSolutionSoFar = Cons(Pick.toInt(pieceId.piece, position), builtSolutionSoFar),
            remainingPositions = remainingPositions &~ incompatiblePositions,
            positionsTakenSoFar = positionsTakenSoFar + position.pos,
            firstLevel = false)
        }
      }

      val positionFlow: Flowable[Position] =
        fromIterable(remainingPositions.filter(pos => pos >= minPosition.pos).map(pos => Position(pos)))
      if (firstLevel)
        positionFlow.flatMapInParallel(solutionsForPick)
      else
        positionFlow.flatMap(solutionsForPick)
    }
  }
}

