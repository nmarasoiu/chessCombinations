package chess

object GenerationCore {
  def solutions(input: Input): Stream[Solution] = {
    input match {
      case Input(table: Table, piecesCount: Map[ChessPiece, Int], positions: Set[Position]) => {
        val s: Stream[Stream[Solution]] =
          for (piece: ChessPiece <- piecesCount.keySet.toStream if piecesCount(piece) > 0;
               position: Position <- positions;
               remainingPiecesCount: Map[ChessPiece, Int] = piecesCount.updated(piece, piecesCount(piece) - 1);
               remainingPositions: Set[Position] = positions - position -- piece.attackPositions(position, input.table);
               smallerInput: Input = Input(table, remainingPiecesCount, remainingPositions))
            yield {
              val s1: Stream[Solution] = solutions(smallerInput)
              s1
            }
        val fl: Stream[Solution] = s.flatten
        fl
      }
    }
  }
}

case class Table(horiz: Int, vert: Int)

case class Input(table: Table,
                 piecesCount: Map[ChessPiece, Int],
                 positions: Set[Position])

case class Solution(solution: Stream[(ChessPiece, Position)])