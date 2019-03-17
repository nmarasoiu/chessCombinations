package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import io.reactivex.schedulers.Schedulers

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
    //    maybeSubscribeAsync(input,
    Flowable.fromCallable(() => {
      val pieces = piecesCountAndMinPosition
      pieces.headOption match {
        case None =>
          just(piecesInPositionsSoFar)
        case Some((piece, (pieceCount, minPosition))) =>
          val positionsToConsider: Positions = positions.from(minPosition)

          val positionFlowable: Flowable[Position] = FlowableUtils.fromIterable(positionsToConsider)

          val positionAndIncompatibilitiesFlowable: Flowable[(Position, Positions)] =
            positionFlowable.map(position => {
              val incompatiblePositions = piece.incompatiblePositions(position, table)
              (position, incompatiblePositions)
            })

          positionAndIncompatibilitiesFlowable
            .filter {
              case (_, incompatiblePositions) =>
                (takenPositionsSoFar & incompatiblePositions).isEmpty
            }
            .flatMap {
              case (position: Position, incompatiblePositions) =>
                val remainingPieces = if (pieceCount == 1)
                  pieces - piece
                else
                  pieces + (piece -> (pieceCount - 1, position + 1))
                val remainingPositions = positions &~ incompatiblePositions
                val newPiecesInPositions = piecesInPositionsSoFar + PiecePosition.toInt(piece, position)
                val newTakenPositions = takenPositionsSoFar + position
                val deeperSolutionPath =
                  SolutionPath(table, remainingPieces, remainingPositions, newPiecesInPositions, newTakenPositions)
                val flowable = deeperSolutionPath.solutions()
                val taskSize = remainingPositions.size.toLong * (1 + remainingPieces.size)

                if (taskSize >= minTaskSize)
                  flowable.subscribeOn(Schedulers.computation())
                else
                  flowable
            }
      }
    }).flatMap(flow => /*maybeSubscribeAsync(input, */ flow) //)
    //    )
  }
}

