package chess

import java.time.Clock
import java.util
import java.util.concurrent._
import java.util.stream
import java.util.stream.Collectors

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.functions.BiFunction

object BlockingUtil {

  case class BufferSize(size: Int) {
    assert(size > 0)
  }

  case class PrintEvery(size: Int) {
    assert(size > 0)
  }
  
  val bufferSize: BufferSize = BufferSize(64)
  val printEvery: PrintEvery = PrintEvery(5000000)

  def blockingTest(table: Table, pieces: Map[Piece, Int], duplicationAssertion: Boolean): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val solutionsFlowable: Flowable[PartialSolution] = SolutionPath.solutions(table, pieces.mapValues(c => Count(c)))

    val solutionCount: Long =
      if (duplicationAssertion) {
        def pickInt(pick: Pick): Int = {
          val Pick(piece, position) = pick
          (position.positionInt << three) + piece.pieceIndex
        }

        case class Sol(picks: Array[Pick]) extends Iterable[Pick] {
          override lazy val hashCode: Int = util.Arrays.hashCode(picks.map(pickInt))

          override def iterator: Iterator[Pick] = picks.iterator
        }

        object Sol {
          def apply(solution: PartialSolution): Sol = Sol(solution.picks.toArray.sortBy(pickInt))
        }

        val solFlowable: Flowable[Sol] =
          solutionsFlowable
            .buffer(bufferSize.size)
            .mapInParallel {
              solutions: util.List[PartialSolution] =>
                val solTs: stream.Stream[Sol] = solutions.stream().map(solution => Sol(solution))
                solTs.collect(Collectors.toList[Sol])
            }.flatMap(lst => Flowable.fromIterable(lst))

        type Solutions = util.HashSet[Sol]
        val seedFactory: Callable[Solutions] = () => new util.HashSet[Sol](50000000)
        //todo sequence to compute this after upstream since the cpu is filled already by upstream parallelism; change the hardcoded value to the count,
        //todo and check why we have so much garbage collection
        val folder: BiFunction[Solutions, Sol, Solutions] = {
          case (solutions: Solutions, solT: Sol) =>
            assert(solutions.add(solT))
            if (solutions.size % printEvery.size == 1)
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
