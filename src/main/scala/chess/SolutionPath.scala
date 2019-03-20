package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import io.reactivex.schedulers.Schedulers
import org.roaringbitmap.RoaringBitmap._

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap

case class SolutionPath(table: Table,
                        piecesCountAndMinPosition: SortedMap[Piece, (PieceCount, Position)],
                        positions: Positions,
                        piecesInPositionsSoFar: Solution,
                        takenPositionsSoFar: Positions) {

  def solutions(): Flowable[Solution] = {
    /**
      * todos
      * - huge GC overhead - perhaps due to testing
      * - lack of sustainable parallelism
      * - hot-spotting on test activity
      * - efficient structure for both tail and size
      */
    val pieces = piecesCountAndMinPosition
    pieces.headOption match {
      case None =>
        just(piecesInPositionsSoFar)
      case Some((piece, (pieceCount, minPosition))) =>
        val positionFlowable: Flowable[Position] =
          FlowableUtils.fromIterable(
            positions.asScala
              .filter(pos => pos >= minPosition)
              .map(_.toInt))
        val positionAndIncompatibilitiesFlowable: Flowable[(Position, Positions)] =
          positionFlowable.map(position => (position, piece.incompatiblePositions(position, table)))

        positionAndIncompatibilitiesFlowable
          .filter {
            case (_, incompatiblePositions) =>
              and(takenPositionsSoFar, incompatiblePositions).isEmpty
          }
          .flatMap {
            case (position: Position, incompatiblePositions) =>
              val remainingPieces = if (pieceCount == 1)
                pieces - piece
              else
                pieces + (piece -> (pieceCount - 1, position + 1))
              val remainingPositions = andNot(positions, incompatiblePositions)
              val newPiecesInPositions = IntListCons(PiecePosition.toInt(piece, position), piecesInPositionsSoFar)
              val newTakenPositions = add(takenPositionsSoFar, position.toLong, position + 1)
              val deeperSolutionPath = SolutionPath(table, remainingPieces, remainingPositions, newPiecesInPositions, newTakenPositions)
              val flowable = deeperSolutionPath.solutions()
              maybeAsync(remainingPieces, remainingPositions, flowable)
          }
    }
  }

  private def maybeAsync(remainingPieces: SortedMap[Piece, (PieceCount, Position)],
                         remainingPositions: Positions,
                         flowable: Flowable[Solution]): Flowable[Solution] = {
    val taskSize = remainingPositions.getLongCardinality * (1 + remainingPieces.size)

    if (taskSize >= minTaskSize)
      flowable.subscribeOn(Schedulers.computation())
    else
      flowable
  }
}

