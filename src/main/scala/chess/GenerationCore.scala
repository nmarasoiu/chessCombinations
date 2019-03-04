package chess

object GenerationCore {
  //todo in parallel? threadsafe? .par but..
  def solutions(input: Input): Iterable[PotentialSolution] = {
    val Input(table, pieces, positions) = input
    if (pieces.isEmpty || table.vert <= 0 || table.horiz <= 0) {
      Seq()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      for (position <- positions;
           remainingPositions = positions - position -- piece.incompatPositions(position, table);
           smallerInput = Input(table, remainingPieces, remainingPositions);
           PotentialSolution(pairs) <- solutions(smallerInput))
        yield PotentialSolution(Stream.cons((piece, position), pairs))
    }
  }
}

case class Input(table: Table,
                 pieces: Seq[ChessPiece],
                 positions: Set[Position])

object Input {
  def piecesFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horiz;
         j <- 0 until table.vert) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[ChessPiece, Int]): Seq[ChessPiece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }

  def apply(table: Table, piecesCount: Map[ChessPiece, Int]): Input = apply(table, toSeq(piecesCount))

  def apply(table: Table, pieces: Seq[ChessPiece]): Input = Input(table, pieces, piecesFor(table).toSet)
}
