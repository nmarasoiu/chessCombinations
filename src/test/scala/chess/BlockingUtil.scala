package chess

import java.time.Clock
import java.util
import java.util.concurrent._
import java.util.stream
import java.util.stream.Collectors

import io.reactivex.Flowable
import io.reactivex.functions.BiFunction
import io.reactivex.parallel.ParallelFlowable

import scala.collection.immutable.BitSet
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
        final case class SolT(solution: Solution) extends Iterable[Pick]{
          private lazy val bitSet: Positions = BitSet(solution.toList: _*)
          private lazy val picks: Array[Long] = bitSet.toBitMask

          override lazy val hashCode: Int = util.Arrays.hashCode(picks)

          override def equals(obj: Any): Boolean = Utils.equals(obj, (other: SolT) => util.Arrays.equals(other.picks, picks))

          override def iterator: Iterator[Pick] = bitSet.iterator
        }

        val solTFlowable: ParallelFlowable[util.List[SolT]] =
          FlowableUtils
            .parallel(solutionsFlowable.buffer(Config.bufferSize))
            .map((solutions: util.List[Solution]) => {
              val solTs: stream.Stream[SolT] = solutions.stream().map(solution => SolT(solution))
              solTs.collect(Collectors.toList[SolT])
            })

        type Solutions = mutable.Set[SolT]
        val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT]
        val folder: BiFunction[Solutions, SolT, Solutions] = {
          case (solutions: Solutions, solT: SolT) =>
            assert(solutions.add(solT))
            if (solutions.size % Config.printEvery == 1)
              print(input.table, solT)
            solutions
        }
        solTFlowable
          .sequential()
          .flatMap(solTList => Flowable.fromIterable(solTList))
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
