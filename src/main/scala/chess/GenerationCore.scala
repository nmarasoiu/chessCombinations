package chess

import io.reactivex.Flowable
import io.reactivex.schedulers.Schedulers

import scala.collection.AbstractIterable
import scala.collection.immutable.Map

object GenerationCore {
  def solutions(input: Input): Flowable[PotentialSolution] = {
    printBeforeAndAfter({
      _solutions(input, picksSoFar = List(), minPositionByPiece = Map[Piece, Position]().withDefaultValue(0))
//        .subscribeOn(Schedulers.computation())
    })
  }

  private def _solutions(input: Input, picksSoFar: List[PiecePosition], minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    flowableFrom({
      val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input
      if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
        Flowable
          .fromArray(PotentialSolution(picksSoFar))
      } else {
        val (piece, pieceCount) = pieces.min
        val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
        __solutions(table, picksSoFar, piece, positions, remainingPieces, minPositionByPiece)
      }
    })
  }

  private def __solutions(table: Table,
                          picksSoFar: List[PiecePosition],
                          piece: Piece,
                          positions: Positions,
                          remainingPieces: OrderedPiecesWithCount,
                          minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    flowableFrom({
      val iterable: Iterable[Flowable[PotentialSolution]] =
        for (positionInt: Position <- toIterable(positions.from(minPositionByPiece(piece)));
             positionPair = Position.fromIntToPair(positionInt, table)
             if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
          yield {
            val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
            val remainingInput = Input(table, remainingPieces, remainingPositions)
            val remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, positionInt + 1)
            val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
            val stream = _solutions(remainingInput, newPicks, remainingMinPosByPiece)
            /*if(remainingPositions.size * remainingPieces.values.sum > 99)
              stream.subscribeOn(Schedulers.computation()).observeOn(Schedulers.computation())
            else*/
              stream
          }
      Flowable.fromIterable({
        import scala.collection.JavaConverters._
        iterable.asJava
      })
        .flatMap(stream => stream)
    })
  }

  private def flowableFrom[T](lazyEvalObservable: => Flowable[T]): Flowable[T] = {
    Flowable.fromCallable(() => lazyEvalObservable)
//      .observeOn(Schedulers.computation())
      .flatMap(stream => stream)
  }

  class WrapperIterable[T](underlying: Iterable[T]) extends AbstractIterable[T] {
    override def iterator: Iterator[T] = underlying.iterator
  }

  def toIterable[T](underlying: Iterable[T]): Iterable[T] = {
    new WrapperIterable[T](underlying)
  }

  private def printlnAndFlush(any: => Any): Unit = {
    println(any)
    Console.flush()
  }

  private def printBeforeAndAfter[T](code: => T): T = {
    printlnAndFlush("Computing..")
    val result = code
    printlnAndFlush("Computing..ended")
    result
  }
}