package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.Map

case class SolutionPath(input: Input,
                        piecesInPositionsSoFar: Solution,
                        takenPositionsSoFar: Positions,
                        minPositionByPiece: Map[Piece, Position]) {

  def solutions(): Flowable[Solution] = {

    def maybeSubscribeAsync[T](remainingInput: Input, flowable: Flowable[T]): Flowable[T] = {
      def taskSize(remainingInput: Input): Long = {
        remainingInput.positions.size.toLong * (1 + remainingInput.pieces.size)
      }

      if (taskSize(remainingInput) >= minTaskSize)
        flowable.subscribeOn(Schedulers.computation())
      else
        flowable
    }

    /**
      * todos
      * - huge GC overhead - perhaps due to testing
      * - lack of sustainable parallelism
      * - hot-spotting on test activity
      * - efficient structure for both tail and size
      */
    //    maybeSubscribeAsync(input,
    Flowable.fromCallable(() => {
      val Input(table, pieces, positions) = input
      pieces.headOption match {
        //      Utils.minOptional(pieces) match {
        case None =>
          just(piecesInPositionsSoFar)
        case Some((piece, pieceCount)) =>
          val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
          val positionsToConsider: Positions = positions.from(minPositionByPiece(piece))

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
                val remainingMinPosByPiece = minPositionByPiece.updated(piece, position + 1)
                val remainingPositions = positions &~ incompatiblePositions
                val remainingInput = Input(table, remainingPieces, remainingPositions)
                val newPiecesInPositions = piecesInPositionsSoFar + PiecePosition.toInt(piece, position)
                val newTakenPositions = takenPositionsSoFar + position
                val deeperSolutionPath = SolutionPath(remainingInput, newPiecesInPositions, newTakenPositions, remainingMinPosByPiece)
                maybeSubscribeAsync(input, deeperSolutionPath.solutions())
            }
      }
    }).flatMap(flow => /*maybeSubscribeAsync(input, */ flow) //)
    //    )
  }
}

