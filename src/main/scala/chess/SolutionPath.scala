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
    minOption(remainingPieces) match {
      case None =>
        just(builtSolutionSoFar)
      case Some((pieceId: PieceId, (PieceCount(count), Position(minPosition)))) =>
        def solutionsForPick(position: Position): Flowable[Solution] = {
          val Position(positionInt) = position
          val positionInTable = PositionInTable(table, position)
          val incompatiblePositions = Piece.of(pieceId).incompatiblePositions(positionInTable)
          if (positionsTakenSoFar.intersects(incompatiblePositions)) {
            empty
          } else {
            val newRemainingPieces = count match {
              case 1 => remainingPieces - pieceId
              case _ => remainingPieces + (pieceId -> (PieceCount(count - 1), Position(positionInt + 1)))
            }
            solutions(
              remainingPieces = newRemainingPieces,
              builtSolutionSoFar = Cons(Pick.toInt(pieceId.piece, position), builtSolutionSoFar),
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

  def minOption[V](map: Map[PieceId, V]): Option[(PieceId, V)] = {
    val none: Option[(PieceId, V)] = None
    map.foldLeft(none) {
      case (None, kv) => Some(kv)
      case (Some((k1, v1)), (k2, v2)) => if (PieceId.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
    }
  }
}

