package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import org.roaringbitmap.RoaringBitmap._

import scala.collection.immutable.SortedMap

case class SolutionPath(table: Table,
                        piecesCountAndMinPosition: SortedMap[Piece, (PieceCount, Position)],
                        positions: Positions,
                        piecesInPositionsSoFar: Solution,
                        takenPositionsSoFar: Positions,
                        firstLevel: Boolean) {

  def solutions(): Flowable[Solution] = {
    piecesCountAndMinPosition.headOption match {
      case None =>
        just(piecesInPositionsSoFar)
      case Some((piece, (count, minPosition))) =>
        Flowable.fromIterable(positions)
          .filter(pos => pos >= minPosition)
          .flatMap(FlowableUtils.asRxFunction(flatMapperFunction(piecesCountAndMinPosition, piece, count)))
    }
  }

  private def flatMapperFunction(pieces: SortedMap[Piece, (PieceCount, Position)], piece: Piece, pieceCount: PieceCount)
                                (position: Position): Flowable[Solution] = {
    val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
    val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1, position + 1))
    val newTakenPositions = andNot(takenPositionsSoFar, incompatiblePositions)
    if (newTakenPositions.getCardinality < takenPositionsSoFar.getCardinality) {
      Flowable.empty[Solution]
    } else {
      newTakenPositions.add(position)
      val remainingPositions = andNot(positions, incompatiblePositions)
      val newPiecesInPositions = IntListCons(PiecePosition.toInt(piece, position), piecesInPositionsSoFar)
      val deeperSolutionPath = SolutionPath(table, remainingPieces, remainingPositions,
        newPiecesInPositions, newTakenPositions, firstLevel = false)
      deeperSolutionPath.solutions()
    }
  }
}

