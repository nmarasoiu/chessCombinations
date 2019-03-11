package chess

import java.lang

import io.reactivex.Flowable
import io.reactivex.Flowable.{empty, just}
import io.reactivex.schedulers.Schedulers

import scala.collection.AbstractIterable
import scala.collection.immutable.Map

object GenerationCore {
  def solutions(input: Input): Flowable[PotentialSolution] = {
    if (input.table.vertical <= 0 || input.table.horizontal <= 0)
      empty()
    else
      _solutions(input, picksSoFar = List(), minPositionByPiece = Map[Piece, Position]().withDefaultValue(0))
  }

  private def _solutions(input: Input, picksSoFar: List[PiecePosition], minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    Flowable.fromIterable(asJava(underlyingSolutions(input, picksSoFar, minPositionByPiece)))
      .flatMap(a => a)
  }

  def minOptional[K, V](map: Map[K, V])(implicit cmp: Ordering[K]): Option[(K, V)] = {
    val none: Option[(K, V)] = None
    map.foldLeft(none) {
      case (None, kv) => Some(kv)
      case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
    }
  }

  private def underlyingSolutions(input:Input,
                                  picksSoFar: List[PiecePosition],
                                  minPositionByPiece: Map[Piece, Position]): Iterable[Flowable[PotentialSolution]] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input
    minOptional(pieces) match {
      case None =>
        Iterable(just(PotentialSolution(picksSoFar)))
      case Some((piece, pieceCount)) =>
        val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
        for (positionInt: Position <- toIterable(positions.from(minPositionByPiece(piece)));
             positionPair = Position.fromIntToPair(positionInt, table)
             if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
          yield {
            val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, positionInt + 1)
            val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
            val taskSize = remainingPositions.size.toLong * remainingPieces.size
            val subSolutions = _solutions(remainingInput, newPicks, remainingMinPosByPiece)
            if (taskSize > 99) subSolutions.subscribeOn(Schedulers.computation()) else subSolutions
          }
    }
  }

  private def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = {
    import scala.collection.JavaConverters._
    scalaIterable.asJava
  }

  class WrapperIterable[T](underlying: Iterable[T]) extends AbstractIterable[T] {
    override def iterator: Iterator[T] = underlying.iterator
  }

  def toIterable[T](underlying: Iterable[T]): Iterable[T] = {
    new WrapperIterable[T](underlying)
  }

}