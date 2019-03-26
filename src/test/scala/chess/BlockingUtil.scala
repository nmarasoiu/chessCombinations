package chess

import java.time.Clock
import java.util
import java.util.concurrent.Callable

import chess.Enrichments._
import chess.FlowableUtils._
import com.google.common.math.Quantiles
import io.reactivex.Flowable
import io.reactivex.functions.BiFunction

import scala.collection.JavaConverters._
import scala.util.Random

object BlockingUtil {

  val bufferSize: BufferSize = BufferSize(1024)
  val printEvery: PrintEvery = PrintEvery(5000000)

  def blockingTest(table: Table, pieces: Map[Piece, Int],
                   duplicationAssertion: Boolean=true, executeTimes: Int,
                   countAssertion: Long => Boolean): Unit = {

    var sumTimes: Double = 0D
    var countTimes = 0
    var secondsSeq = List[Double]()

    println(s"Computing $executeTimes times for $table $pieces $duplicationAssertion..")

    val clock = Clock.systemUTC()

    for (_ <- 1 to executeTimes) {
      val t0 = clock.instant()

      val solutionsFlowable = SolutionPath.solutions(table, pieces.mapValues(c => Count(c)))

      val solutionCount: Long =
        if (duplicationAssertion) {
          val solFlowable: Flowable[Sol] =
            solutionsFlowable
              .buffer(bufferSize.size)
              .mapInParallel(solutions => solutions.asScala.map(Sol(_)))
              .flatMap(iterable => fromIterable(iterable))

          type Solutions = util.HashSet[Sol]
          val seedFactory: Callable[Solutions] = () => new util.HashSet[Sol]()
          //to do sequence to compute this after upstream since the cpu is filled already by upstream parallelism; change the hardcoded value to the count,
          //to do and check why we have so much garbage collection
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

        }
        else {
          solutionsFlowable.count().blockingGet()
        }

      val t1 = clock.instant()

      val duration = java.time.Duration.between(t0, t1)
      val seconds = duration.toNanos.toDouble / 1e9
      sumTimes += seconds
      countTimes += 1
      println(" computed in " + duration + " -> " + solutionCount + " solutionFlowable found," +
        " with running average=" + (sumTimes / countTimes))
      secondsSeq = seconds :: secondsSeq
      val percentiles = Quantiles.percentiles()
        .indexes(0 to 100:_*)
        .compute(secondsSeq
          .map(_.doubleValue.asInstanceOf[java.lang.Double])
          .asJavaCollection)
        .asScala.toMap
        .toSortedMap
      println(percentiles)
      assert(countAssertion(solutionCount))
      Thread.sleep(math.abs(Random.nextLong() % 543))
    }
  }

  case class Sol(picks: Array[Pick]) {
    override lazy val hashCode: Int = util.Arrays.hashCode(picks.asInstanceOf[Array[Object]])
  }

  object Sol {
    def apply(solution: SubSolution): Sol = Sol(solution.picks.toArray.sorted)
  }

  case class BufferSize(size: Int) {
    assert(size > 0)
  }

  case class PrintEvery(size: Int) {
    assert(size > 0)
  }

}
