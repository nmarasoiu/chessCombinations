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
                        piecesCountAndMinPosition: SortedMap[Piece, (PieceCount, Position)],
                        firstLevel: Boolean) {

  private val empty: Flowable[Solution] = Flowable.empty[Solution]

  def solutions(): Flowable[Solution] =
    piecesCountAndMinPosition.headOption match {
      case None =>
        just(builtSolutionSoFar)
      case Some((piece, (count, minPosition))) =>
        fromIterable(iterable(remainingPositions, minPosition))
          .flatMapInParallel(firstLevel)(flatMapperFunction(piecesCountAndMinPosition, piece, count))
    }


  def iterable(positions: Positions, minPosition: Position): Iterable[Position] =
    positions.asScala.filter(pos => pos >= minPosition).map(_.toInt)

  private def flatMapperFunction(pieces: SortedMap[Piece, (PieceCount, Position)], piece: Piece, pieceCount: PieceCount)
                                (position: Position): Flowable[Solution] = {
    val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
    val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1, position + 1))
    val newTakenPositions = andNot(positionsTakenSoFar, incompatiblePositions)
    if (newTakenPositions.getCardinality < positionsTakenSoFar.getCardinality) {
      empty
    } else {
      newTakenPositions.add(position)
      val nextStepSolutionPath = SolutionPath(table,
        remainingPositions = andNot(remainingPositions, incompatiblePositions),
        builtSolutionSoFar = PiecePositionIntListCons(PiecePosition.toInt(piece, position), builtSolutionSoFar),
        positionsTakenSoFar = newTakenPositions,
        piecesCountAndMinPosition = remainingPieces,
        firstLevel = false)
      nextStepSolutionPath.solutions()
    }
  }
}

