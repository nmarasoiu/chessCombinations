package chess

object GenerationCore {
  //todo in parallel? threadsafe? .par but..
  def solutions(input: Input): Iterable[PotentialSolution] = {
    _solutions(input, Set(), "").filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position], prefix: String): Iterable[PotentialSolution] = {
    def sout(msg: Any): Unit = println(prefix + msg)

    val Input(table, pieces, positions) = input
    if (pieces.isEmpty || table.vert <= 0 || table.horiz <= 0) {
      Seq()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      sout("Picked piece=" + piece + ", remaining=" + remainingPieces)
      positions.flatMap(position => {
        sout("Picked position=" + position)
        val incompatiblePositions = piece.incompatPositions(position, table)
        val remainingPositions = positions - position -- incompatiblePositions
        sout("remainingPositions=" + remainingPositions)

        if (remainingPieces.isEmpty) {
          val itIsOkToPickThisPieceInThisPosition = picksSoFar.intersect(incompatiblePositions).isEmpty
          if (itIsOkToPickThisPieceInThisPosition) {
            val potentialSolution = PotentialSolution(Set((piece, position)))
            sout(" SOLUTION=" + potentialSolution)
            Seq(potentialSolution)
          } else {
            sout("NOT A SOLUTION: already picked positions: " + picksSoFar +
              " are having an intersection with the incompatible positions of " + piece + " within " + table
              + " which are " + incompatiblePositions)
            Seq()
          }
        } else {
          val remainingInput = Input(table, remainingPieces, remainingPositions)
          val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar + position, prefix + "  ")
          sout("remainingPotentialSolutions=" + remainingPotentialSolutions)

          remainingPotentialSolutions.map(remainingPotentialSolution => {
            val potentialSolution = PotentialSolution(Set((piece, position)) ++ remainingPotentialSolution.solution)
            sout("potentialSolution=" + potentialSolution)
            potentialSolution
          })
        }
      })
    }
  }
}

case class Input(table: Table,
                 pieces: Seq[Piece],
                 positions: Set[Position])

object Input {
  def piecesFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horiz;
         j <- 0 until table.vert) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }

  def apply(table: Table, piecesCount: Map[Piece, Int]): Input = apply(table, toSeq(piecesCount))

  def apply(table: Table, pieces: Seq[Piece]): Input = Input(table, pieces, piecesFor(table).toSet)
}
