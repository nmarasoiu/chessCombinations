package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.Flowable.just
import org.roaringbitmap.RoaringBitmap._

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap

case class SolutionPath(table: Table,
                        remainingPositions: Positions,
                        builtSolutionSoFar: Solution,
                        positionsTakenSoFar: Positions,
                        remainingPieces: SortedMap[Piece, (PieceCount, Position)],
                        firstLevel: Boolean) {

  private val empty: Flowable[Solution] = Flowable.empty[Solution]

  def solutions(): Flowable[Solution] =
    remainingPieces.headOption match {
      case None =>
        just(builtSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        fromIterable(iterable(remainingPositions, minPosition))
          .flatMapInParallel(firstLevel)(flatMapperFunction(remainingPieces, piece, count))
    }

  def iterable(positions: Positions, minPosition: Position): Iterable[Position] =
    positions.asScala.filter(pos => pos >= minPosition).map(_.toInt)

  private def flatMapperFunction(pieces: SortedMap[Piece, (PieceCount, Position)], piece: Piece, pieceCount: PieceCount)
                                (position: Position): Flowable[Solution] = {
    val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
    val positionsTakenSoFarWhichAreStillCompatibleWithTheNewChoice = andNot(positionsTakenSoFar, incompatiblePositions)
    if (positionsTakenSoFarWhichAreStillCompatibleWithTheNewChoice.getCardinality < positionsTakenSoFar.getCardinality) {
      empty
    } else {
      val newTakenPositions = positionsTakenSoFarWhichAreStillCompatibleWithTheNewChoice //a clone of positionsTakenSoFar
      newTakenPositions.add(position)

      val nextStepSolutionPath = SolutionPath(table,
        positionsTakenSoFar = newTakenPositions,
        remainingPositions = andNot(remainingPositions, incompatiblePositions),
        builtSolutionSoFar = PiecePositionIntListCons(PiecePosition.toInt(piece, position), builtSolutionSoFar),
        remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1, position + 1)),
        firstLevel = false)

      nextStepSolutionPath.solutions()
    }
  }
}

