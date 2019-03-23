package chess

import java.time.Clock
import java.util
import java.util.concurrent._
import java.util.stream
import java.util.stream.Collectors

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.functions.BiFunction

import scala.collection.mutable

object BlockingUtil {
  def blockingTest(table: Table, pieces: Map[Piece, Int], duplicationAssertion: Boolean = true): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val solutionsFlowable: Flowable[PartialSolution] = SolutionPath.solutions(table, pieces.mapValues(c => Count(c)))

    val solutionCount: Long =
      if (duplicationAssertion) {

        case class Sol(picks: Array[Pick]) extends Iterable[Pick] {
          override lazy val hashCode: Int = util.Arrays.hashCode(picks.map(_.pickInt))

          override def iterator: Iterator[Pick] = picks.iterator
        }

        object Sol {
          def apply(solution: PartialSolution): Sol = Sol(solution.picks.toArray.sortBy(_.pickInt))
        }

        val solFlowable: Flowable[Sol] =
          solutionsFlowable
            .buffer(Config.bufferSize.size)
            .mapInParallel {
              solutions: util.List[PartialSolution] =>
                val solTs: stream.Stream[Sol] = solutions.stream().map(solution => Sol(solution))
                solTs.collect(Collectors.toList[Sol])
            }.flatMap(lst => Flowable.fromIterable(lst))

        type Solutions = mutable.Set[Sol]
        val seedFactory: Callable[Solutions] = () => new mutable.HashSet[Sol]
        val folder: BiFunction[Solutions, Sol, Solutions] = {
          case (solutions: Solutions, solT: Sol) =>
            assert(solutions.add(solT))
            if (solutions.size % Config.printEvery.size == 1)
              print(table, solT)
            solutions
        }

        solFlowable
          .reduceWith(seedFactory, folder)
          .blockingGet()
          .size

      } else {
        solutionsFlowable.count().blockingGet()
      }

    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + solutionCount + " solutionFlowable found")

    solutionCount
  }

  private def print[T](table: Table, solution: Iterable[Pick]): Unit = {
    println(
      (for (pick <- solution)
        yield PickTest.fromIntToPieceAndCoordinates(pick, table)
        ).toIndexedSeq.sortBy(_.piece))
  }
}
