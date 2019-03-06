package chess

object GenerationCore {
  /**
    * todo:
    * proceed in the order of pieces that eliminate more positions, to minimize recursive search space
    * in parallel: course grained, split the table, do .par on some collections, careful on granularity
    * scala test, add tests
    * refactor into a single for and get rid of flatten?
    * inspect & remove todos
    * refactoring, beautiful well organized code
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
