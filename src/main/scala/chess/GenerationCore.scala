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
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input
    if (pieces.isEmpty) {
      just(PotentialSolution(picksSoFar))
    } else {
      __solutions(table, picksSoFar, positions, pieces, minPositionByPiece)
    }
  }

  private def __solutions(table: Table,
                          picksSoFar: List[PiecePosition],
                          positions: Positions,
                          pieces: OrderedPiecesWithCount,
                          minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    Flowable.just(())
      .flatMapIterable(_ => asJava(underlyingSolutions(table, picksSoFar, positions, pieces, minPositionByPiece)))
      .flatMap {
        case (taskSize: Long, childFlowable: Flowable[PotentialSolution]) =>
          if (taskSize >= 90) childFlowable.subscribeOn(Schedulers.computation()) else childFlowable
      }
  }

  private def underlyingSolutions(table: Table,
                                  picksSoFar: List[PiecePosition],
                                  positions: Positions,
                                  pieces: OrderedPiecesWithCount,
                                  minPositionByPiece: Map[Piece, Position]): Iterable[(Long, Flowable[PotentialSolution])] = {
    val (piece, pieceCount) = pieces.min
    val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
    for (positionInt: Position <- toIterable(positions.from(minPositionByPiece(piece)));
         positionPair = Position.fromIntToPair(positionInt, table)
         if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
      yield {
        val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
        val remainingInput = Input(table, remainingPieces, remainingPositions)
        val remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, positionInt + 1)
        val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
        (remainingPositions.size.toLong * remainingPieces.size, _solutions(remainingInput, newPicks, remainingMinPosByPiece))
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