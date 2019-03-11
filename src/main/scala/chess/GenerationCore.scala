package chess

import io.reactivex.Flowable
import io.reactivex.schedulers.Schedulers

import scala.collection.AbstractIterable
import scala.collection.immutable.Map

object GenerationCore {
  //  val devMode: Boolean = sys.env.get("DEV_MODE").exists(txt => txt.trim.equalsIgnoreCase("true"))

  def solutions(input: Input): Flowable[PotentialSolution] = {
    _solutions(input)(List())(Map[Piece, Position]().withDefaultValue(0))
  }

  private def _solutions(input: Input)(picksSoFar: List[PiecePosition])(minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    val Input(table, pieces: OrderedPiecesWithCount, positions: Positions) = input

    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Flowable.fromArray(PotentialSolution(picksSoFar))
    } else {
      val (piece, pieceCount) = pieces.min
      val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1))
      __solutions(table, picksSoFar, piece, positions, remainingPieces, minPositionByPiece)
    }
  }

  def __solutions(table: Table,
                  picksSoFar: List[PiecePosition],
                  piece: Piece,
                  positions: Positions,
                  remainingPieces: OrderedPiecesWithCount,
                  minPositionByPiece: Map[Piece, Position]): Flowable[PotentialSolution] = {
    val iterable: Iterable[Flowable[PotentialSolution]] =
      for (positionInt: Position <- toIterable(positions.from(minPositionByPiece(piece)));
           positionPair = Position.fromIntToPair(positionInt, table)
           if !picksSoFar.exists { case PiecePosition(_, otherPosition) => piece.takes(positionPair, otherPosition) })
        yield {
          val remainingPositions = positions &~ piece.incompatiblePositions(positionPair._1, positionPair._2, table)
          val remainingInput = Input(table, remainingPieces, remainingPositions)
          val remainingMinPosByPiece: Map[Piece, Position] = minPositionByPiece.updated(piece, positionInt + 1)
          val newPicks = PiecePosition(piece, positionPair) :: picksSoFar
          val observable = _solutions(remainingInput)(newPicks)(remainingMinPosByPiece)

          if (remainingPieces.values.sum * remainingPositions.size >= 150)
            observable
              .subscribeOn(Schedulers.computation())
              .observeOn(Schedulers.computation())
          else
            observable
        }

    Flowable.fromIterable({
      import scala.collection.JavaConverters._
      iterable.asJava
    })
      .flatMap(stream => stream)
  }

  def toIterable(set: Positions): Iterable[Position] = {
    new AbstractIterable[Position] {
      override def iterator: Iterator[Position] = {
        set.iterator
      }
    }
  }
}