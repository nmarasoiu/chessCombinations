package chess

object GenerationCore {
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Set[PotentialSolution] = {
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position]): Set[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Set[Position]) = input
    if (pieces.isEmpty || table.vert <= 0 || table.horiz <= 0) {
      Set()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      //todo refactor into a single for and get rid of flatten?
      val r = for (position <- positions;
                   incompatiblePositions = piece.incompatPositions(position, table);
                   _ <- Set(1) if incompatiblePositions.intersect(picksSoFar).isEmpty;
                   remainingPositions = positions - position -- incompatiblePositions)
        yield
          if (remainingPieces.isEmpty) {
            val potentialSolution = PotentialSolution(Set((piece, position)))
            Seq(potentialSolution)
          } else {
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar + position)

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
                 pieces: Seq[Piece],//with duplicates
                 positions: Set[Position])

object Input {
  def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
    apply(table, toSeq(piecesCount), positionsFor(table).toSet)

  def positionsFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horiz; j <- 0 until table.vert) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }
}

case class Position(x: Int, y: Int)

case class Table(horiz: Int, vert: Int)

case class PotentialSolution(solution: Set[(Piece, Position)])
