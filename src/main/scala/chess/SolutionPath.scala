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
    val Input(table, pieces, positions) = input
    val maybeTuple = Utils.minOptional(pieces)

    maybeTuple match {
      case None =>
        just(piecesInPositionsSoFar)
      case Some((piece, pieceCount)) =>
        val positionsToConsider: Positions = positions.from(minPositionByPiece(piece))

        val positionFlowable: Flowable[Position] = FlowableUtils.fromIterable(positionsToConsider)

        val positionAndIncompatibilitiesFlowable: Flowable[(Position, Positions)] =
          positionFlowable.map(position => {
            val incompatiblePositions = piece.incompatiblePositions(position, table)
            (position, incompatiblePositions)
          })

        val solutionWithSizeFlowable: Flowable[(Long, Flowable[Solution])] =
          positionAndIncompatibilitiesFlowable
            .filter {
              case (_: Position, incompatiblePositions: Positions) =>
                (takenPositionsSoFar & incompatiblePositions).isEmpty
            }
            .map {
              case (position: PiecePositionInt, incompatiblePositions: Positions) =>
                val remainingMinPosByPiece: Map[Piece, PiecePositionInt] = minPositionByPiece.updated(piece, position + 1)
                val remainingPositions = positions &~ incompatiblePositions
                val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
                val remainingInput = Input(table, remainingPieces, remainingPositions)
                val newPiecesInPositions = piecesInPositionsSoFar + PiecePosition.toInt(piece, position)
                val newTakenPositions = takenPositionsSoFar + position
                val taskSize: Long = remainingPositions.size.toLong * (1 + remainingPieces.size)
                val deeperSolutionPath = SolutionPath(remainingInput, newPiecesInPositions, newTakenPositions, remainingMinPosByPiece)
                val subSolutionFlowable = deeperSolutionPath.solutions()
                (taskSize, subSolutionFlowable)
            }

        solutionWithSizeFlowable.flatMap {
          case (taskSize: Long, subSolutions: Flowable[Solution]) =>
            if (taskSize > minTaskSize)
              subSolutions.subscribeOn(Schedulers.computation())
            else
              subSolutions
        }
    }
  }

}
