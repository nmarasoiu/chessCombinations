package chess

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object GenerationCore {
  private implicit val ec = ExecutionContext.global

  /**
    * todo:
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
  def solutions(input: Input): Iterable[PotentialSolution] = {
    val eventualSolutions: Future[Iterable[Future[PotentialSolution]]] = _solutions(input)(Set())(Map().withDefaultValue(0))
    eventualSolutions.foreach(x => println("overall future: " + x))
    eventualSolutions.foreach(_.foreach(x => println("each future: " + x)))
    eventualSolutions.foreach(_.foreach(_.foreach(x => println("each solution: " + x))))
    await(eventualSolutions)
      .map(s => await(s))
//      .filter(sol => sol.solution.size == input.pieces.size)
  }

  def await[T](future: Future[T]): T = Await.result(future, Duration.Inf)

  private def _solutions(input: Input)(picksSoFar: Set[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Future[Iterable[Future[PotentialSolution]]] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: Seq[Piece]): Future[Iterable[Future[PotentialSolution]]] = {
      val futureSolutions: Iterable[Future[Iterable[Future[PotentialSolution]]]] =
        for (position: Position <- positions if position >= minPositionForPiece && !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(position, otherPosition) };
             incompatiblePositions: Positions = piece.attackPositions(position, table);
             remainingPositions: Positions = positions - position &~ incompatiblePositions;
             remainingInput: Input = Input(table, remainingPieces, remainingPositions);
             remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, position + 1);
             newPicks: Set[PiecePosition] = picksSoFar + PiecePosition(piece, position))
          yield _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
      Future.successful(Future.sequence(futureSolutions).map(ss => ss.flatten)).flatten
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Future.successful(Seq(Future.successful(PotentialSolution(picksSoFar))))
    } else {
      val piece: Piece = pieces.head
      val minPositionForPiece = minPositionByPiece(piece)
      val remainingPieces: Seq[Piece] = pieces.tail
      lazy val eventualSolutionsSupplier = __solutions(piece, minPositionForPiece, remainingPieces)
      if (remainingPieces.size > 2 && remainingPieces.size * positions.size > 40) {
        Future(eventualSolutionsSupplier).flatten
      } else {
        eventualSolutionsSupplier
      }
    }
  }
}