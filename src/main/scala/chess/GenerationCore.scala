package chess

object GenerationCore {
  //todo in parallel? threadsafe? .par but..
  def solutions(input: Input): Seq[PotentialSolution] = {
    val Input(table, piecesCount, positions) = input
    (for (piece <- piecesCount.keySet.toSeq if piecesCount(piece) > 0;
          position <- positions;
          remainingPiecesCount = piecesCount.updated(piece, piecesCount(piece) - 1);
          remainingPositions = positions - position -- piece.incompatPositions(position,table);
          smallerInput = Input(table, remainingPiecesCount, remainingPositions))
      yield
        for (PotentialSolution(pairs) <- solutions(smallerInput))
          yield PotentialSolution(Stream.cons((piece, position), pairs))
      ).flatten
  }
}

case class Input(table: Table,
                 piecesCount: Map[ChessPiece, Int],
                 positions: Set[Position])

object Input {
  def piecesFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horiz;
         j <- 0 until table.vert) yield Position(i, j)
  }

  def apply(table: Table, piecesCount: Map[ChessPiece, Int]): Input = Input(table, piecesCount, piecesFor(table).toSet)
}
