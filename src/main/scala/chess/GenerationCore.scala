package chess

object GenerationCore {
  //todo in parallel? threadsafe? .par but..
  def solutions(input: Input, picksSoFar: Seq[Position], prefix: String): Iterable[PotentialSolution] = {
    def sout(msg: Any): Unit = println(prefix + msg)

    val Input(table, pieces, positions) = input
    if (pieces.isEmpty || table.vert <= 0 || table.horiz <= 0) {
      Seq()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      sout("Picked piece=" + piece + ", remaining=" + remainingPieces)
      /*
      val r = for (position <- positions;
                   sout("Picked position=" + position);
                   remainingPositions = positions - position -- piece.incompatPositions(position, table);
                   sout("remainingPositions=" + remainingPositions);
                   remainingInput = Input(table, remainingPieces, remainingPositions);
                   sout("remainingInput=" + remainingInput);
                   remainingPotentialSolution <- solutions(remainingInput);
                   sout("remainingPotentialSolution=" + remainingPotentialSolution);
                   potentialSolution = PotentialSolution((piece, position) :: remainingPotentialSolution.solution);
                   sout("potentialSolution=" + potentialSolution))
        yield potentialSolution*/
      positions.flatMap(position => {
        sout("Picked position=" + position)
        val remainingPositions = positions - position -- piece.incompatPositions(position, table)
        sout("remainingPositions=" + remainingPositions)

        if (remainingPositions.isEmpty) {
          if (piece.incompatPositions(position, table).toSet.intersect(picksSoFar.toSet).isEmpty) {
            val potentialSolution = Seq(PotentialSolution(List((piece, position))))
            sout("SOLUTION=" + potentialSolution)
            potentialSolution
          } else {
            sout("NOT A SOLUTION: already picked positions: " + picksSoFar +
              " are having an intersection with the incompatible positions of " + piece + " within " + table
              + " which are " + piece.incompatPositions(position, table))
            Seq()
          }
        } else {
          val remainingInput = Input(table, remainingPieces, remainingPositions)
          val remainingPotentialSolutions = solutions(remainingInput, picksSoFar ++ Seq(position), prefix + "  ")
          sout("remainingPotentialSolutions=" + remainingPotentialSolutions)

          remainingPotentialSolutions.map(remainingPotentialSolution => {
            val potentialSolution = PotentialSolution((piece, position) :: remainingPotentialSolution.solution)
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
