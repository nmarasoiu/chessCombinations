package chess

object GenerationCore {
  def solutions(input: Input): Stream[Solution] = {
    val Input(table, piecesCount, positions) = input
    (for (piece <- piecesCount.keySet.toStream if piecesCount(piece) > 0;
          position <- positions;
          remainingPiecesCount = piecesCount.updated(piece, piecesCount(piece) - 1);
          remainingPositions = positions - position -- piece.attackPositions(position, input.table);
          smallerInput = Input(table, remainingPiecesCount, remainingPositions))
      yield {
        for(Solution(pairs)<-solutions(smallerInput)) yield Solution(Stream.cons((piece,position), pairs))
      }).flatten
  }
}

case class Table(horiz: Int, vert: Int)

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

case class Solution(solution: Stream[(ChessPiece, Position)])

