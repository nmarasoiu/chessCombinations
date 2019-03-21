package chess

import io.reactivex.Flowable
import io.reactivex.Flowable.empty
import io.reactivex.schedulers.Schedulers
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.SortedMap

object GenerationCore {

  def solutions(input: Input): Flowable[Solution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      SolutionPath(input.table, pack(input.pieces), input.positions,
        piecesInPositionsSoFar = EmptyList,
        takenPositionsSoFar = new RoaringBitmap(),
        level = 0)
        .solutions()
//        .subscribeOn(Schedulers.computation())
//        .observeOn(Schedulers.computation())//?
  }

  def pack(pieces: SortedMap[Piece, PieceCount]): SortedMap[Piece, (PieceCount, Position)] =
    pieces.mapValues(count => (count, 0))

}