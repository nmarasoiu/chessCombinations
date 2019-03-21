package chess

import java.time.Clock
import java.util
import java.util.concurrent._
import java.util.stream
import java.util.stream.Collectors

import io.reactivex.Flowable
import io.reactivex.functions.BiFunction
import io.reactivex.parallel.ParallelFlowable

import scala.collection.mutable

object BlockingUtil {

  def blockingIterable(input: Input): Iterable[Solution] = FlowableUtils.blockToIterable(GenerationCore.solutions(input))

  def blockingTest(table: Table, piecesToPositions: Map[Piece, Position]): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val input = Input.from(table, piecesToPositions)
    val solutionsFlowable: Flowable[Solution] = GenerationCore.solutions(input)

    final case class SolT(piecePositions: Array[Int]) {
      override val hashCode: Int = util.Arrays.hashCode(piecePositions)
    }
    object SolT {
      def apply(solution: Solution): SolT = SolT(solution.toList.toArray.sorted)
    }

    val solTFlowable: ParallelFlowable[util.List[SolT]] =
      FlowableUtils
        .parallel(solutionsFlowable.buffer(32))
        .map((solutions: util.List[Solution]) => {
          val solTs: stream.Stream[SolT] = solutions.stream().map(solution => SolT(solution))
          solTs.collect(Collectors.toList[SolT])
        })

    type Solutions = mutable.Set[SolT]
    val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT]
    val folder: BiFunction[Solutions, SolT, Solutions] = {
      case (solutions: Solutions, solution: SolT) =>
        assert(solutions.add(solution))
        if (solutions.size % 5000000 == 1)
          print(input, solution.piecePositions)
        solutions
    }

    val solutionCount =
      solTFlowable
        .sequential()
        .flatMap(solTList => Flowable.fromIterable(solTList))
        .reduceWith(seedFactory, folder)
        .blockingGet()
        .size

    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + solutionCount + " solutionFlowable found")

    solutionCount
  }

  private def print(input: Input, solution: IndexedSeq[Int]): Unit = {
    println(
      (for (piecePosition <- solution)
        yield PiecePosition.fromIntToPieceAndCoordinates(piecePosition, input.table)
        ).toIndexedSeq.sortBy(_.piece))
  }

  def priorityDecreasingThreadFactory(factory: ThreadFactory): ThreadFactory = {
    runnable: Runnable => {
      val thread = factory.newThread(runnable)
      thread.setPriority(Thread.MIN_PRIORITY)
      thread
    }
  }
}
