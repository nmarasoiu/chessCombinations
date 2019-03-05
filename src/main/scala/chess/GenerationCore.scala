package chess

object GenerationCore {
  /**
    * todo:
    * in parallel course grained, split the table
    * scala test, add tests
    * profile with their bigger example
    * anything to cache/reuse, dynamic prog?
    * refactor into a single for and get rid of flatten?
    * inspect & remove todos
    * refactoring, beautiful well organized code
    * memory pressure, balance lazy with eager
    */
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Set[PotentialSolution] = {
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position]): Set[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Set[Position]) = input
    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Set()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      //todo refactor into a single for and get rid of flatten?
      val r = for (position <- positions;
                   incompatiblePositions = piece.incompatiblePositions(position, table);
                   _ <- Set(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
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
    for (i <- 0 until table.horizontal; j <- 0 until table.vertical) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }
}

case class Position(x: Int, y: Int)

case class Table(horizontal: Int, vertical: Int)

case class PotentialSolution(solution: Set[(Piece, Position)])
