package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.empty
import org.roaringbitmap.RoaringBitmap

object GenerationCore {

  def solutions(input: Input): Flowable[Solution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else {
      val rootSolutionPath = SolutionPath(input.table, remainingPositions = input.positions, builtSolutionSoFar = EmptyList$PiecePosition, positionsTakenSoFar = new RoaringBitmap(), piecesCountAndMinPosition = input.pieces.mapValues(count => (count, 0)), firstLevel = true)
      rootSolutionPath.solutions()
    }
  }

}