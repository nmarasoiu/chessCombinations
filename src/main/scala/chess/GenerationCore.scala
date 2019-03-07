package chess


import monix.reactive.Observable

import scala.concurrent.duration.Duration
import scala.concurrent.{Future}

object GenerationCore {
  /**
    * todo:
    * use bitset in set of positions too
    * use async testing or apply this in tests: https://monix.io/docs/2x/best-practices/blocking.html#if-blocking-use-scalas-blockcontext
    * error management on the async
    * introduce reactive streams to replace Future+Seq and free up memory; or Seq[Future[x;
    * can i convert both ways between Future[Seq and Seq[Future ? .sequence is right to left but in reverse don't think it is possible?
    * use async/await
    * print as you go
    * scala test (replacing or in addition to scalascheck), add edge-case tests
    * more refactoring, beautiful well organized code; document the trade-offs
    * collect warnings with codestyle, pmd, findbug
    * check for a healthy way to create an immutable version/copy of the mutable bitset
    */
  def solutions(input: Input): Observable[PotentialSolution] = {
    val observable = _solutions(input)(Set())(Map().withDefaultValue(0))
    import monix.execution.Scheduler.Implicits.global
    observable.foreach(println)
    observable
  }

  private def _solutions(input: Input)(picksSoFar: Set[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Observable[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: Seq[Piece]): Observable[PotentialSolution] = {
      val observables: Iterable[Observable[PotentialSolution]] =
        for (position: Position <- positions if position >= minPositionForPiece && !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(position, otherPosition) };
             incompatiblePositions: Positions = piece.attackPositions(position, table);
             remainingPositions: Positions = positions - position &~ incompatiblePositions;
             remainingInput: Input = Input(table, remainingPieces, remainingPositions);
             remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, position + 1);
             newPicks: Set[PiecePosition] = picksSoFar + PiecePosition(piece, position))
          yield _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
      Observable.fromIterable(observables).flatten
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Observable(PotentialSolution(picksSoFar))
    } else {
      val piece: Piece = pieces.head
      val minPositionForPiece = minPositionByPiece(piece)
      val remainingPieces: Seq[Piece] = pieces.tail
      lazy val eventualSolutions = __solutions(piece, minPositionForPiece, remainingPieces)
      if (remainingPieces.size > 2 && remainingPieces.size * positions.size > 40) {
        import monix.execution.Scheduler.Implicits.global
        Observable(Future(eventualSolutions)).mapFuture(a => a).flatten
      } else {
        eventualSolutions
      }
    }
  }
}