package chess

import chess.Utils._
import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}

import scala.collection.immutable.{BitSet, Map}

object SolutionPath {

  def solutions(input: Input): Flowable[Solution] = {
    val table = input.table
    if (table.vertical <= 0 || table.horizontal <= 0)
      empty()
    else
      SolutionPath(table).solutions(
        remainingPositions = input.positions,
        builtSolutionSoFar = EmptyList$PiecePosition,
        remainingPieces = input.pieces.mapValues(count => (count, 0)),
        positionsTakenSoFar = BitSet(),
        firstLevel = true)
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
      case Some((piece, (pieceCount, minPosition))) =>
        fromIterable(remainingPositions.filter(pos => pos >= minPosition))
          .flatMapInParallel(firstLevel) {
            position =>
              val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
              //newTakenPositions is a clone of positionsTakenSoFar minus (andNot) incompatiblePositions, meaning positionsTakenSoFarWhichAreStillCompatibleWithTheNewChoice
              val newTakenPositions = positionsTakenSoFar &~ incompatiblePositions //diff
              if (newTakenPositions.size < positionsTakenSoFar.size) {
                empty
              } else {
                solutions(
                  remainingPieces = if (pieceCount == 1) remainingPieces - piece else remainingPieces + (piece -> (pieceCount - 1, position + 1)),
                  builtSolutionSoFar = PickListCons(PiecePosition.toInt(piece, position), builtSolutionSoFar),
                  remainingPositions = remainingPositions &~ incompatiblePositions, //diff
                  positionsTakenSoFar = newTakenPositions + position,
                  firstLevel = false)
              }
          }
    }
  }
}

