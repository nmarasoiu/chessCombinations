package chess

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object GenerationCore {
  /**
    * todo:
    * - remove OOM
    * - error management on the async
    * in parallel: course grained, split the table, do .par on some collections, careful on granularity
    * scala test, add tests
    * refactor into a single for and get rid of flatten?
    * refactoring, beautiful well organized code
    * done - proceed in the order of pieces that eliminate more positions, to minimize recursive search space
    */
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Seq[PotentialSolution] = {
    Await.result(_solutions(input)(Set())(Map().withDefaultValue(0)), Duration.Inf)
      .filter(sol => sol.solution.size == input.pieces.size)

  }

  val parallelExecutions = new AtomicInteger()
  val maxParallelExecutions = Runtime.getRuntime.availableProcessors()

  private def _solutions(input: Input)(picksSoFar: Set[Position])(minPositionByPiece: Map[Piece, Position]): Future[Seq[PotentialSolution]] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input
    implicit val ec = ExecutionContext.global

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: Seq[Piece]): Future[Seq[PotentialSolution]] = {
      val r: Seq[Future[Seq[PotentialSolution]]] =
        for (position: PositionInt <- positions.filter(pos => pos >= minPositionForPiece).toSeq; //todo try BitSet.range intersect with positions?
             incompatiblePositions = piece.attackPositions(position, table);
             _ <- Seq(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
             remainingPositions = positions - position &~ incompatiblePositions)
          yield {
            val piecePosition = PiecePosition(piece, position)
            if (remainingPieces.isEmpty) {
              Future.successful(Seq(PotentialSolution(Set(piecePosition))))
            } else {
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val remainingPotentialSolutions: Future[Seq[PotentialSolution]] =
                _solutions(remainingInput)(picksSoFar + position)(minPositionByPiece.updated(piece, position + 1))

              remainingPotentialSolutions.map(_.map(remainingPotentialSolution => {
                PotentialSolution(remainingPotentialSolution.solution + piecePosition)
              }))
            }
          }
      Future.sequence(r).map(_.flatten)
    }

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Future.successful(Seq())
    } else {
      val piece: Piece = pieces.head
      val minPositionForPiece = minPositionByPiece(piece)
      val remainingPieces: Seq[Piece] = pieces.tail
      val eventualSolutionsSupplier: () => Future[Seq[PotentialSolution]] = () => __solutions(piece, minPositionForPiece, remainingPieces)
      if (remainingPieces.size > 1 && positions.size > 9 && parallelExecutions.incrementAndGet() <= maxParallelExecutions) {
        val future = Future(eventualSolutionsSupplier.apply()).flatten
        future.onComplete(_ => parallelExecutions.decrementAndGet())
        future
      } else {
        eventualSolutionsSupplier.apply()
      }

    }
  }
}