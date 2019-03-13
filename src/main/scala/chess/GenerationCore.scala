package chess

import java.lang

import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.{BitSet, Map}

object GenerationCore {
  def solutions(input: Input): Flowable[Solution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      _solutions(input,
        piecesInPositionsSoFar = BitSet(),
        takenPositionsSoFar = BitSet(),
        minPositionByPiece = Map().withDefaultValue(0))
  }
//todo method object
  private def _solutions(input: Input, piecesInPositionsSoFar: Solution, takenPositionsSoFar: Solution, minPositionByPiece: Map[Piece, Position]): Flowable[Solution] = {
    val Input(table, pieces, positions) = input
    minOptional(pieces) match {
      case None =>
        just(piecesInPositionsSoFar)
      case Some((piece, pieceCount)) =>
        Flowable
          .fromIterable(asJava(positions.from(minPositionByPiece(piece))))
          //todo try rx scala for comprehension
          .flatMap(position => {
            val incompatiblePositions: Positions = piece.incompatiblePositions(position, table)
            if ((takenPositionsSoFar & incompatiblePositions).nonEmpty) {
              empty[Solution]()
            } else {
              val remainingMinPosByPiece = minPositionByPiece.updated(piece, position + 1)
              val remainingPositions = positions &~ incompatiblePositions
              val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val newPiecesInPositions = piecesInPositionsSoFar + PiecePosition.toInt(piece, position)
              val newTakenPositions = takenPositionsSoFar + position
              val taskSize = remainingPositions.size.toLong * (1 + remainingPieces.size)
              val subSolutions = _solutions(remainingInput, newPiecesInPositions, newTakenPositions, remainingMinPosByPiece)
              if (taskSize > minTaskSize)
                subSolutions.subscribeOn(Schedulers.computation())
              else
                subSolutions
            }
          })
    }
  }

  def minOptional[K, V](map: Map[K, V])(implicit cmp: Ordering[K]): Option[(K, V)] = {
    val none: Option[(K, V)] = None
    map.foldLeft(none) {
      case (None, kv) => Some(kv)
      case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
    }
  }

  private def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = {
    import scala.collection.JavaConverters._
    scalaIterable.asJava
  }

}