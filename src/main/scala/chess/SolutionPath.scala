package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.Map

case class SolutionPath(input: Input,
                        piecesInPositionsSoFar: Solution,
                        takenPositionsSoFar: Positions,
                        minPositionByPiece: Map[PieceInt, Position]) {

  def solutions(): Flowable[Solution] = {
    val Input(table, pieces, positions) = input
    if (pieces.isEmpty) {
      just(piecesInPositionsSoFar)
    } else {
      val (piece: Piece, remainingPieces: Iterable[PieceInt]) = (Piece.values(pieces.head), pieces.tail)
      val positionsToConsider: Positions = positions.from(minPositionByPiece(piece.order))

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
            val remainingMinPosByPiece = minPositionByPiece.updated(piece.order, position + 1)
            val remainingPositions = positions &~ incompatiblePositions
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val newPiecesInPositions = piecesInPositionsSoFar + PiecePosition.toInt(piece, position)
            val newTakenPositions = takenPositionsSoFar + position
            val taskSize = remainingPositions.size.toLong * (1 + remainingPieces.size)
            val deeperSolutionPath = SolutionPath(remainingInput, newPiecesInPositions, newTakenPositions, remainingMinPosByPiece)
            val subSolutions = deeperSolutionPath.solutions()
            if (taskSize >= minTaskSize)
              subSolutions.subscribeOn(Schedulers.computation())
            else
              subSolutions
        }
    }
  }
}

