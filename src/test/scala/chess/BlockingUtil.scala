package chess

import java.time.Clock
import java.util.concurrent.Callable

import io.reactivex.functions.BiFunction
import io.reactivex.schedulers.Schedulers

import scala.collection.mutable


object BlockingUtil {
  def blockingIterable(input: Input): Iterable[Solution] = FlowableUtils.toIterable(GenerationCore.solutions(input))

  def blockingTest(table: Table, piecesToPositions: Map[Piece, Position], checkDuplication: Boolean = false): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0nano = System.nanoTime
    val t0 = clock.instant()

    val input = Input.from(table, piecesToPositions)
    val solutionsFlowable = GenerationCore.solutions(input).subscribeOn(Schedulers.computation())
    val solutionCount: Long =
      if (checkDuplication) {
        final case class SolT(mask: mutable.IndexedSeq[Long]) {
          override lazy val hashCode: Int = super.hashCode()
        }
        object SolT {
          def apply(solution: Solution): SolT = SolT(solution.toBitMask.to[mutable.WrappedArray])
        }
        type Solutions = mutable.Set[SolT]
        val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT]
        val folder: BiFunction[Solutions, Solution, Solutions] = {
          case (solutions: Solutions, solution: Solution) =>
            assert(solutions.add(SolT(solution)))
            if (solutions.size % 10000 == 1) print(input, solution)
            solutions
        }
        // blocks
        solutionsFlowable.reduceWith(seedFactory, folder).blockingGet().size
      } else {
        solutionsFlowable.count.blockingGet()
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
