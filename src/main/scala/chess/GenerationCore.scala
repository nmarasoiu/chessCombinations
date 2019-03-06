package chess

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * I still have two major things to do, which I am doing now, and I will send them as soon as I can:
  * - remove permutations & rotations! impose an order on the piecePostion list of the solution, to only search in that order; was: to figure out a way to not recompute solutions multiple times (which will improve orders of magnitude)
  * (ideas: caching, dynamic programming, also for each solution there are other 3 solutions which just rotate the table when square)
  * - to allow for multi-core / hyper-threaded execution with coarse-grained "tasks" (also improve performance )
  */
object GenerationCore {
  /**
    * todo:
    * check why we compute duplicates! try caching or dynamic programming because we recompute solutions too much
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

  private def _solutions(input: Input)(picksSoFar: Set[Position])(minPositionByPiece: Map[Piece, Position]): Future[Seq[PotentialSolution]] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input

    def __solutions(piece: Piece, minPositionForPiece: Position, remainingPieces: Seq[Piece]) = {
      implicit val ec = ExecutionContext.global
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
      implicit val ec = ExecutionContext.global
      Future(__solutions(piece, minPositionForPiece, remainingPieces)).flatten
    }
  }
}