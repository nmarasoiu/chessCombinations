package chess

import monix.execution.Scheduler.{Implicits => monixImplicits}
import monix.reactive.Observable

import scala.collection.immutable.Map
import scala.concurrent.Future

object GenerationCore {
  val devMode: Boolean = sys.env.get("DEV_MODE").exists(txt => txt.trim.equalsIgnoreCase("true"))

  /**
    * todo:
    * gc thrashing: check out removing Position class, and revert to the functions in Position object converting between (Int,Int) positions and Int positions (for BitSet)
    * taking a minimum, ideally constant amount of memory per level, so that the program can run in decent Xmx e.g. use Iterables not Seq, BitSet not Set etc
    * gc too much - sometimes a sinusoidal, while for other ones flat between 9% - 21% - investigate further
    * check parallelism / granularity of tasks: remainingPieces>2 may be too small for some configs & thrashing is too much, gc spiking
    * i have about 20% GC, to investigate the causes of thrashing
    * add edge-case tests with zero and negative numbers/dimensions etc
    * i got 2 different results for the number of the solutions for the 7x7 problem, both a bit over 10M but still different...
    * document the trade-offs between performance / memory / type power (e.g. avoided a Position(Int,Int) case class which also took care of Position Int <-> (Int,Int) conversion via companion object, in favor of  two functions to convert directly without allocating memory..maybe it should be put back for a better model?)
    * collect warnings with codestyle, pmd, findbug, sonar, qr & fix them
    * any explicit error management on the async/Observable ?
    * done?: use async testing? or apply this in tests?: https://monix.io/docs/2x/best-practices/blocking.html#if-blocking-use-scalas-blockcontext (currently blocking in tests seems in an ok way)
    */
  def solutions(input: Input): Observable[PotentialSolution] = {
    val observable = _solutions(input)(Set())(Map[Piece, Position]().withDefaultValue(0))
    if (devMode) {
      import monixImplicits.global
      observable.foreach(println)
    }
    observable
  }

  private def _solutions(input: Input)(picksSoFar: Set[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Observable[PotentialSolution] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input

    def recursion(piece: Piece, remainingPieces: OrderedPiecesWithCount, position: Position): Observable[PotentialSolution] = {
      val remainingPositions = positions &~ piece.incompatiblePositions(position, table)
      val remainingInput = Input(table, remainingPieces, remainingPositions)
      val remainingMinPosByPiece = minPositionByPiece.updated(piece, position + 1)
      val newPicks = picksSoFar + PiecePosition(piece, position)
      _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)
    }

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: OrderedPiecesWithCount): Observable[PotentialSolution] = {
      Observable.fromIterable(
        for (positionInt: Int <- positions.from(minPositionForPiece).toStream
             if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionInt, otherPosition, table) })
          yield recursion(piece, remainingPieces, positionInt)).flatten
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Observable(PotentialSolution(picksSoFar))
    } else {
      val (piece, pieceCount) = pieces.min
      val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
      import monixImplicits.global
      Observable(Future(__solutions(piece, minPositionByPiece(piece), remainingPieces))).mapFuture(a => a).flatten
    }
  }
}