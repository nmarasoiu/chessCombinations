package chess

object GenerationCore {
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Iterable[PotentialSolution] = {
    _solutions(input, Seq(), "").filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Seq[Position], prefix: String): Iterable[PotentialSolution] = {
    val Input(table, pieces, positions: Set[Position]) = input
    if (pieces.isEmpty || table.vert <= 0 || table.horiz <= 0) {
      Seq()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      //todo refactor into a single for and get rid of flatten?
      val r = for (position <- positions;
                   incompatiblePositions: Set[Position] = piece.incompatPositions(position, table);
                   _ <- Set(1) if incompatiblePositions.intersect(picksSoFar.toSet).isEmpty;
                   remainingPositions = positions - position -- incompatiblePositions)
        yield
          if (remainingPieces.isEmpty) {
            val potentialSolution = PotentialSolution(Set((piece, position)))
            Seq(potentialSolution)
          } else {
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar ++ Seq(position), prefix + "  ")

            remainingPotentialSolutions.map(remainingPotentialSolution => {
              val potentialSolution = PotentialSolution(Set((piece, position)) ++ remainingPotentialSolution.solution)
              potentialSolution
            })
          }
      r.flatten
    }
  }

}

case class Input(table: Table,
                 pieces: Seq[Piece],
                 positions: Set[Position])

object Input {
  def positionsFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horiz;
         j <- 0 until table.vert) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }

  def apply(table: Table, piecesCount: Map[Piece, Int]): Input = apply(table, toSeq(piecesCount))

  def apply(table: Table, pieces: Seq[Piece]): Input = Input(table, pieces, positionsFor(table).toSet)
}
