package chess

import monix.execution.Scheduler.{Implicits => monixImplicits}
import monix.reactive.Observable

import scala.collection.immutable.Map
import scala.concurrent.Future

object GenerationCore {
  val devMode:Boolean = sys.env.get("DEV_MODE").exists(txt => txt.trim.equalsIgnoreCase("true"))
  /**
    * todo:
    * i have about 20% GC, to investigate the causes of thrashing
    * add edge-case tests with zero and negative numbers/dimensions etc
    * i got 2 different results for the number of the solutions for the 7x7 problem, both a bit over 10M but still different...
    * document the trade-offs between performance / memory / type power (e.g. avoided a Position(Int,Int) case class which also took care of Position Int <-> (Int,Int) conversion via companion object, in favor of  two functions to convert directly without allocating memory..maybe it should be put back for a better model?)
    * collect warnings with codestyle, pmd, findbug, sonar, qr & fix them
    * any explicit error management on the async/Observable ?
    * done?: use async testing? or apply this in tests?: https://monix.io/docs/2x/best-practices/blocking.html#if-blocking-use-scalas-blockcontext (currently blocking in tests seems in an ok way)
    */
  def solutions(input: Input): Observable[PotentialSolution] = {
    val observable = _solutions(input)(Set())(Map[Piece, Position]().withDefaultValue(Position.zero))
    if(devMode) {
      import monixImplicits.global
      observable.foreach(println)
    }
    observable
  }

  private def _solutions(input: Input)(picksSoFar: Set[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Observable[PotentialSolution] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: OrderedPiecesWithCount): Observable[PotentialSolution] = {
      val observables: Iterable[Observable[PotentialSolution]] =
        for (position: Position <- positions.toIndexedSeq.map(Position(_)) if position.xy >= minPositionForPiece.xy
          && !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(position, otherPosition) };
             incompatiblePositions: Positions = piece.attackPositions(position, table);
             remainingPositions: Positions = positions &~ incompatiblePositions;
             remainingInput: Input = Input(table, remainingPieces, remainingPositions);
             remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, Position(position.xy + 1));
             newPicks: Set[PiecePosition] = picksSoFar + PiecePosition(piece, position))
          yield _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
      Observable.fromIterable(observables).flatten
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Observable(PotentialSolution(picksSoFar))
    } else {
      val (piece, pieceCount) = pieces.min
      val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
      lazy val eventualSolutions = __solutions(piece, minPositionByPiece(piece), remainingPieces)
      if (remainingPieces.values.sum > 2) {
        import monixImplicits.global
        Observable(Future(eventualSolutions)).mapFuture(a => a).flatten
      } else {
        eventualSolutions
      }
    }
  }
}