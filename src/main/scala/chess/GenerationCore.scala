package chess

import monix.execution.Scheduler.{Implicits => monixImplicits}
import monix.reactive.Observable

import scala.collection.immutable.Map
import scala.concurrent.Future

object GenerationCore {
  val devMode: Boolean = sys.env.get("DEV_MODE").exists(txt => txt.trim.equalsIgnoreCase("true"))

  def solutions(input: Input): Observable[PotentialSolution] = {
    val observable = _solutions(input)(List())(Map[Piece, Position]().withDefaultValue(0))
    if (devMode) {
      import monixImplicits.global
      observable.foreach(println)
    }
    observable
  }

  private def _solutions(input: Input)(picksSoFar: List[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Observable[PotentialSolution] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: OrderedPiecesWithCount): Observable[PotentialSolution] = {
      Observable.fromIterable(
        for (positionInt: Int <- positions.from(minPositionForPiece).toStream;
             positionPair = Position.fromIntToPair(positionInt, table)
             if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
          yield {
            val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingMinPosByPiece: Map[Piece, Int] = minPositionByPiece.updated(piece, positionInt + 1)
            val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
            _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
          }).flatten
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Observable(PotentialSolution(picksSoFar))
    } else {
      val (piece, pieceCount) = pieces.min
      val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
      lazy val eventualSolutions = __solutions(piece, minPositionByPiece(piece), remainingPieces)

      val minRemainingPieceCountNoDuplicates: Double = 1
      val minRemainingPieceCountWithDuplicates: Double = 1
      val areaDivisor: Double = 20
      if (remainingPieces.size >= minRemainingPieceCountNoDuplicates && {
        val remainingPiecesCount: Double = remainingPieces.values.sum
        remainingPiecesCount >= minRemainingPieceCountWithDuplicates && {
          val remainingPositionCount: Double = positions.size
          val tableArea: Double = table.horizontal.toDouble * table.vertical
          val minRemainingPositionCount: Double = tableArea / areaDivisor
          remainingPositionCount >= minRemainingPositionCount &&
            remainingPiecesCount * remainingPositionCount >= minRemainingPieceCountWithDuplicates * minRemainingPositionCount * 40
        }
      }) {
        import scala.concurrent.ExecutionContext.Implicits.global
        Observable(Future(eventualSolutions)).mapFuture(a => a).flatten
      }
      else {
        eventualSolutions
      }
    }
  }
}