package chess

object GenerationCore {
  /**
    * todo:
    * replace Set[Position] with BitSet everywhere to make the set construction faster & set-set O(1), i.e. refactoring Piece logic to PieceSet a type alias of BitSet (immutable?)
    * remove/replace more streams: e.g. replace Stream[Piece] with Map[Piece,Int]
    * in parallel: course grained, split the table, do .par on some collections, careful on granularity
    * scala test, add tests
    * anything to cache/reuse, dynamic prog?
    * refactor into a single for and get rid of flatten?
    * inspect & remove todos
    * refactoring, beautiful well organized code
    * memory pressure, balance lazy with eager
    * too many sets? preallocate?
    */
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Set[PotentialSolution] = {
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position]): Set[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Set[PositionInt]) = input
    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Set()
    } else {
      val piece: Piece = pieces.head
      val remainingPieces: Seq[Piece] = pieces.tail
      val r =
        for (position: PositionInt <- positions;
             incompatiblePositions = piece.incompatiblePositions(position, table);
             _ <- Seq(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
             remainingPositions = positions - position -- incompatiblePositions)
          yield {
            val piecePosition = PiecePosition(piece, position)
            if (remainingPieces.isEmpty) {
              val potentialSolution = PotentialSolution(Set(piecePosition))
              Set(potentialSolution)
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
