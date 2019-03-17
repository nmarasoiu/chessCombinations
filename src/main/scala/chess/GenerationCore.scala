package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.empty

import scala.collection.immutable.{BitSet, SortedMap}

object GenerationCore {

  def solutions(input: Input): Flowable[Solution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      SolutionPath(input.table, pack(input.pieces), input.positions,
        piecesInPositionsSoFar = BitSet(),
        takenPositionsSoFar = BitSet()
      )
        .solutions()
  }

  def pack(pieces: SortedMap[Piece, PieceCount]): SortedMap[Piece, (PieceCount, Position)] =
    pieces.mapValues(count => (count,0))

}