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
  def blockingTest(table: Table, pieces: Map[Piece, Position], duplicationAssertion: Boolean = true): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val input = Input.from(table, pieces)
    val solutionsFlowable: Flowable[Solution] = solutions(input)

    val solutionCount: Long =
      if (duplicationAssertion) {

        final case class Sol(picks: Array[Pick]) extends Iterable[Pick] {

          override lazy val hashCode: Int = util.Arrays.hashCode(picks)

          override def equals(obj: Any): Boolean = Utils.equals(obj, (other: Sol) => util.Arrays.equals(other.picks, picks))

          override def iterator: Iterator[Pick] = picks.iterator
        }

        object Sol {
          def apply(solution: Solution): Sol = Sol(solution.toList.toArray.sorted)
        }

        val solFlowable: Flowable[Sol] =
          solutionsFlowable
            .buffer(Config.bufferSize)
            .map2(inParallel = true) {
              solutions: util.List[Solution] =>
                val solTs: stream.Stream[Sol] = solutions.stream().map(solution => Sol(solution))
                solTs.collect(Collectors.toList[Sol])
            }.flatMap(lst => Flowable.fromIterable(lst))

        type Solutions = mutable.Set[Sol]
        val seedFactory: Callable[Solutions] = () => new mutable.HashSet[Sol]
        val folder: BiFunction[Solutions, Sol, Solutions] = {
          case (solutions: Solutions, solT: Sol) =>
            assert(solutions.add(solT))
            if (solutions.size % Config.printEvery == 1)
              print(input.table, solT)
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

  private def solutions(input: Input): Flowable[Solution] = {
    SolutionPath.solutions(table = input.table,
      positions = input.positions, pieces = input.pieces)
  }

  private def print[T](table: Table, solution: Iterable[Pick]): Unit = {
    println(
      (for (pick <- solution)
        yield PiecePosition.fromIntToPieceAndCoordinates(pick, table)
        ).toIndexedSeq.sortBy(_.piece))
  }

  def blockingIterable(input: Input): Iterable[Solution] = blockToIterable(solutions(input))

  import scala.collection.JavaConverters._

  def blockToIterable[T](flowable: Flowable[T]): Iterable[T] = flowable.blockingIterable().asScala
}
