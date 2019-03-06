package chess

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenerationCore {
  /**
    * todos:
    * - error management on the async
    * scala test (replacing or in addition to scalascheck), add edge-case tests
    * refactor into a single for and get rid of flatten: is this possible?
    * more refactoring, beautiful well organized code; document the tradeoffs
    */
  def solutions(input: Input): Seq[PotentialSolution] = {
    Await.result(_solutions(input)(Set())(Map().withDefaultValue(0)), Duration.Inf)
      .filter(sol => sol.solution.size == input.pieces.size)
  }

  private implicit val ec = ExecutionContext.global

  private def _solutions(input: Input)(picksSoFar: Set[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Future[Seq[PotentialSolution]] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: Seq[Piece]): Future[Seq[PotentialSolution]] = {
      val seq: Seq[Future[Seq[PotentialSolution]]] =
        for (position <- positions.toSeq if position >= minPositionForPiece;
             _ <- Seq(1) if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(position, otherPosition) };
             incompatiblePositions = piece.attackPositions(position, table);
             remainingPositions = positions - position &~ incompatiblePositions;
             remainingInput = Input(table, remainingPieces, remainingPositions);
             remainingMinPosByPiece = minPositionByPiece.updated(piece, position + 1);
             newPicks = picksSoFar + PiecePosition(piece, position))
          yield _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
      Future.sequence(seq).map(_.flatten)
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Future.successful(Seq(PotentialSolution(picksSoFar)))
    } else {
      val piece: Piece = pieces.head
      val minPositionForPiece = minPositionByPiece(piece)
      val remainingPieces: Seq[Piece] = pieces.tail
      val eventualSolutionsSupplier: () => Future[Seq[PotentialSolution]] =
        () => __solutions(piece, minPositionForPiece, remainingPieces)
      if (remainingPieces.size > 2 && remainingPieces.size * positions.size > 40) {
        Future(eventualSolutionsSupplier.apply()).flatten
      } else {
        eventualSolutionsSupplier.apply()
      }
    }
  }
}