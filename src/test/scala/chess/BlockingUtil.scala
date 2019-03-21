package chess

import java.time.Clock
import java.util
import java.util.concurrent._

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

      //note: we override equals and canEqual to speed up the checks to almost sure equality
      override def equals(obj: Any): Boolean = {
        obj.isInstanceOf[SolT] && util.Arrays.equals(obj.asInstanceOf[SolT].piecePositions, piecePositions)
      }

    }
    object SolT {
      def apply(solution: Solution): SolT = SolT(solution.toList.toArray.sorted)
    }

    val solTFlowable: ParallelFlowable[SolT] = FlowableUtils.parallel(solutionsFlowable).map(solution => SolT(solution))

    type Solutions = mutable.Set[SolT]
    val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT] // ConcurrentSet.createSet[SolT]
    val folder: BiFunction[Solutions, SolT, Solutions] = {
      case (solutions: Solutions, solution: SolT) =>
        assert(solutions.add(solution))
        if (solutions.size % 5000000 == 1)
          print(input, solution.piecePositions)
        solutions
    }

    val solutionCount: Long =
      solTFlowable
        .sequential()
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
