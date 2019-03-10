package chess

import io.reactivex.Flowable

import scala.collection.immutable.Map

object GenerationCore {
  //  val devMode: Boolean = sys.env.get("DEV_MODE").exists(txt => txt.trim.equalsIgnoreCase("true"))

  def solutions(input: Input): Flowable[PotentialSolution] = {
    _solutions(input)(List())(Map[Piece, Position]().withDefaultValue(0))
  }

  private def _solutions(input: Input)(picksSoFar: List[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: OrderedPiecesWithCount): Flowable[PotentialSolution] = {
      val iterable: Iterable[Flowable[PotentialSolution]] =
        for (positionInt: Position <- positions.from(minPositionForPiece).toStream.toIterable;
             positionPair = Position.fromIntToPair(positionInt, table)
             if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
          yield {
            val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, positionInt + 1)
            val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
            _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
          }
      import scala.collection.JavaConverters._
      Flowable.fromIterable(iterable.asJava).flatMap(a => a)
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Flowable.fromArray(PotentialSolution(picksSoFar))
    } else {
      val (piece, pieceCount) = pieces.min
      val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
      __solutions(piece, minPositionByPiece(piece), remainingPieces)
    }
  }
}