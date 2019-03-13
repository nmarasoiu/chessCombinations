package chess

import java.lang

import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.Map

object GenerationCore {
  def solutions(input: Input): Flowable[PotentialSolution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      _solutions(input, picksSoFar = List(), minPositionByPiece = Map().withDefaultValue(0))
  }

  private def _solutions(input: Input, picksSoFar: List[PiecePosition], minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    val Input(table, pieces, positions) = input
    minOptional(pieces) match {
      case None =>
        just(PotentialSolution(picksSoFar))
      case Some((piece, pieceCount)) =>
        Flowable
          .fromIterable(asJava(positions.from(minPositionByPiece(piece))))
          .flatMap(positionInt => {
            val positionPair@(x: Int, y: Int) = Position.fromIntToPair(positionInt, table)
            if (picksSoFar.exists {
              case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition)
            }) {
              empty[PotentialSolution]()
            } else {
              val remainingMinPosByPiece = minPositionByPiece.updated(piece, positionInt + 1)
              val remainingPositions = positions &~ piece.incompatiblePositions(x, y, table)
              val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
              val taskSize = remainingPositions.size.toLong * (1 + remainingPieces.size)
              val subSolutions = _solutions(remainingInput, newPicks, remainingMinPosByPiece)
              if (taskSize > 150)
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