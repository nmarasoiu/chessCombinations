package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.empty

import scala.collection.immutable.{BitSet, Map}

object GenerationCore {
  def solutions(input: Input): Flowable[Solution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      SolutionPath(input,
        piecesInPositionsSoFar = BitSet(),
        takenPositionsSoFar = BitSet(),
        minPositionByPiece = Map().withDefaultValue(0)
      )
        .solutions()
  }


}