package chess

/**
  * I still have two major things to do, which I am doing now, and I will send them as soon as I can:
  * - to figure out a way to not recompute solutions multiple times (which will improve orders of magnitude)
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
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position]): Seq[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Positions) = input
    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Seq()
    } else {
      val piece: Piece = pieces.head
      val remainingPieces: Seq[Piece] = pieces.tail
      val r: Seq[Iterable[PotentialSolution]] =
        for (position: PositionInt <- positions.toSeq;
             incompatiblePositions = piece.attackPositions(position, table);
             _ <- Seq(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
             remainingPositions = positions - position &~ incompatiblePositions)
          yield {
            val piecePosition = PiecePosition(piece, position)
            if (remainingPieces.isEmpty) {
              Set(PotentialSolution(Set(piecePosition)))
            } else {
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar + position)

              remainingPotentialSolutions.map(remainingPotentialSolution => {
                PotentialSolution(remainingPotentialSolution.solution + piecePosition)
              })
            }
          }
      r.flatten
    }
  }
}
