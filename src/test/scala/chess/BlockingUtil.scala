package chess

import java.time.Clock
import java.util

import chess.FlowableUtils._
import io.reactivex.Flowable

import scala.collection.JavaConverters

object BlockingUtil {

  case class BufferSize(size: Int) {
    assert(size > 0)
  }

  case class PrintEvery(size: Int) {
    assert(size > 0)
  }

  val bufferSize: BufferSize = BufferSize(1024)
  val printEvery: PrintEvery = PrintEvery(5000000)

  var sumTimes: Double = 0D
  var countTimes = 0

  def blockingTest(table: Table, pieces: Map[Piece, Int], duplicationAssertion: Boolean): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val solutionsFlowable = SolutionPath.solutions(table, pieces.mapValues(c => Count(c)))

    val solutionCount: Long =
      if (duplicationAssertion) {
        def pickInt(pick: Pick): Int = {
          val Pick(piece, position) = pick
          (position.positionInt << three) + piece.pieceIndex
        }

        case class Sol(picks: Array[Int]) {
          override lazy val hashCode: Int = util.Arrays.hashCode(picks)
        }

        object Sol {
          def apply(solution: SubSolution): Sol = Sol(solution.picks.toStream.map(pickInt).toArray.sorted)
        }
        import JavaConverters._
        val solFlowable: Flowable[Sol] =
          solutionsFlowable
            .buffer(bufferSize.size)
            .mapInParallel(solutions => solutions.iterator.asScala.map(Sol(_)).toIterable.asJava)
            .flatMap(iterable => Flowable.fromIterable(iterable))
        /*
                type Solutions = util.HashSet[Sol]
                val seedFactory: Callable[Solutions] = () => new util.HashSet[Sol]()
                //to do sequence to compute this after upstream since the cpu is filled already by upstream parallelism; change the hardcoded value to the count,
                //to do and check why we have so much garbage collection
                val folder: BiFunction[Solutions, Sol, Solutions] = {
                  case (solutions: Solutions, solT: Sol) =>
                    if (solutions.isEmpty) //todo delete this if line, add all the time to the set
                      assert(solutions.add(solT))
                    if (solutions.size % printEvery.size == 1)
                      print(table, solT)
                    solutions
                }
        */

        solFlowable
          //          .reduceWith(seedFactory, folder)
          .count()
          .blockingGet()
        //          .size

      }
      else {
        solutionsFlowable.count().blockingGet()
      }

    val t1 = clock.instant()

    val duration = java.time.Duration.between(t0, t1)
    sumTimes += duration.getSeconds
    countTimes += 1
    println(" computed in " + duration + " -> " + solutionCount + " solutionFlowable found," +
      " with running average=" + (sumTimes / countTimes))

    solutionCount
  }

  private def print[T](table: Table, solution: Iterable[Pick]): Unit

  = {
    println(
      (for (pick <- solution)
        yield PickTest.fromIntToPieceAndCoordinates(pick, table)
        ).toIndexedSeq.sortBy(_.piece))
  }
}
