package chess
import io.reactivex.functions.Function
import java.util.concurrent.atomic.AtomicInteger

import io.reactivex.Flowable
import io.reactivex.Flowable.just
import io.reactivex.schedulers.Schedulers
import org.roaringbitmap.RoaringBitmap._

import scala.collection.immutable.SortedMap

case class SolutionPath(table: Table,
                        piecesCountAndMinPosition: SortedMap[Piece, (PieceCount, Position)],
                        positions: Positions,
                        piecesInPositionsSoFar: Solution,
                        takenPositionsSoFar: Positions,
                        level: Int) {

  def solutions(): Flowable[Solution] = {
    piecesCountAndMinPosition.headOption match {
      case None =>
        just(piecesInPositionsSoFar)
      case Some((piece, (count, minPosition))) =>
        lazy val flatMapper = asRxFunction(flatMapperFunction(piecesCountAndMinPosition, piece, count))
        Flowable.fromIterable(positions)
          .filter(pos => pos >= minPosition)
          .flatMap(flatMapper,flatMapConcurrency)
    }
  }
  private def asRxFunction(func:Position=>Flowable[Solution]): Function[Integer, Flowable[Solution]] = {
    position: Integer => func(position)
  }
  private def flatMapperFunction(pieces: SortedMap[Piece, (PieceCount, Position)], piece: Piece, pieceCount: PieceCount)
                                (position: Position): Flowable[Solution] = {
    val (incompatiblePositions, remainingPieces, newTakenPositions) =
      computingForFilterAndFlatMap(pieces, piece, pieceCount, position)
    if (newTakenPositions.getCardinality < takenPositionsSoFar.getCardinality) {
      Flowable.empty[Solution]
    } else {
      val flowable = computingFlowable(piece, position, incompatiblePositions, remainingPieces, newTakenPositions)
      SolutionPath.maybeAsyncSubscribeToSomeInnerFlowables(this, flowable)
    }
  }

  private def computingForFilterAndFlatMap(pieces: SortedMap[Piece, (PieceCount, Position)], piece: Piece,
                                           pieceCount: PieceCount, position: Int)
  : (Positions, SortedMap[Piece, (PieceCount, Position)], Positions) = {
    val incompatiblePositions = piece.incompatiblePositions(PositionInTable(position, table))
    val remainingPieces = if (pieceCount == 1) pieces - piece else pieces + (piece -> (pieceCount - 1, position + 1))
    val newTakenPositions = andNot1(incompatiblePositions)
    (incompatiblePositions, remainingPieces, newTakenPositions)
  }

  private def computingFlowable(piece: Piece, position: Int, incompatiblePositions: Positions, remainingPieces: SortedMap[Piece, (PieceCount, Position)], newTakenPositions: Positions) = {
    newTakenPositions.add(position)
    val remainingPositions = andNot2(incompatiblePositions)
    val newPiecesInPositions = IntListCons(PiecePosition.toInt(piece, position), piecesInPositionsSoFar)
    val deeperSolutionPath = SolutionPath(table, remainingPieces, remainingPositions,
      newPiecesInPositions, newTakenPositions, level + 1)
    val flowable = deeperSolutionPath.solutions()
    flowable
  }

  private def andNot1(incompatiblePositions: Positions): Positions = {
    andNot(takenPositionsSoFar, incompatiblePositions)
  }

  private def andNot2(incompatiblePositions: Positions): Positions = {
    andNot(positions, incompatiblePositions)
  }
}

object SolutionPath {
  private val counter = new AtomicInteger()

  private def maybeAsyncSubscribeToSomeInnerFlowables[T](solutionPath: SolutionPath, flowable: Flowable[T]): Flowable[T] = {
    if (solutionPath.level == 1
//      && solutionPath.positions.getCardinality * (solutionPath.piecesCountAndMinPosition.size + 1) >= minTaskSize
    ) {
      flowable
//        .doOnSubscribe(_ => println(counter.incrementAndGet() + "\tsubscribing"))
        .subscribeOn(Schedulers.computation())
//        .doOnComplete(() => println(counter.decrementAndGet() + "\tcompleted"))
    } else
      flowable
  }

}

