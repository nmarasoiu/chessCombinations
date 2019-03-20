package chess

import java.time.Clock
import java.util.concurrent.Callable

import io.reactivex.Flowable
import io.reactivex.functions.BiFunction
import io.reactivex.schedulers.Schedulers

import scala.collection.mutable


object BlockingUtil {
  def blockingIterable(input: Input): Iterable[Solution] = FlowableUtils.blockToIterable(GenerationCore.solutions(input))

  def blockingTest(table: Table, piecesToPositions: Map[Piece, Position], checkDuplication: Boolean = false): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0nano = System.nanoTime
    val t0 = clock.instant()

    final case class SolT(mask: mutable.IndexedSeq[Int]) {
      override val hashCode: Int = super.hashCode()
    }
    object SolT {
      def apply(solution: Solution): SolT = SolT(solution.toArray.to[mutable.WrappedArray])
    }

    val input = Input.from(table, piecesToPositions)
    val solutionsFlowable = GenerationCore.solutions(input)

    val solutionCount: Long =
      if (checkDuplication) {
        type Solutions = mutable.Set[SolT]
        val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT]
        val folder: BiFunction[Solutions, SolT, Solutions] = {
          case (solutions: Solutions, solution: SolT) =>
            assert(solutions.add(solution))
            //            if (solutions.size % 5000000 == 1) print(input, solution)
            solutions
        }
        // blocks
        val solTFlowable: Flowable[SolT] =
          solutionsFlowable
            .subscribeOn(Schedulers.computation())
            .map(solution => SolT(solution))

        solTFlowable
          .subscribeOn(Schedulers.newThread())
          .reduceWith(seedFactory, folder)
          .blockingGet()
          .size
      } else {
        solutionsFlowable
          .count
          .blockingGet()
      }
    val t1 = clock.instant()
    val t1nano = System.nanoTime
    println(" computed in " + java.time.Duration.between(t0, t1) + " / " +
      ((t1nano.toDouble - t0nano) / 1e9) + " -> " + solutionCount + " solutionFlowable found")

    solutionCount
  }

  private def print(input: Input, solution: Solution): Unit = {
    println(Solution.fromIntToPieceAndCoordinates(solution, input.table))
  }
}
