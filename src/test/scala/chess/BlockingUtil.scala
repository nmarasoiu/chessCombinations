package chess

import java.time.Clock
import java.util
import java.util.concurrent._
import java.util.stream

import io.reactivex.Flowable
import io.reactivex.functions.BiFunction
import io.reactivex.schedulers.Schedulers

import scala.collection.mutable

object BlockingUtil {

  private val mappingExecutor = new ThreadPoolExecutor(1, 8, 1L, TimeUnit.SECONDS,
    new LinkedBlockingQueue[Runnable], priorityDecreasingThreadFactory(Executors.defaultThreadFactory()))

  def blockingIterable(input: Input): Iterable[Solution] = FlowableUtils.blockToIterable(GenerationCore.solutions(input))

  def blockingTest(table: Table, piecesToPositions: Map[Piece, Position]): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val input = Input.from(table, piecesToPositions)
    val solutionsFlowable: Flowable[Solution] = GenerationCore.solutions(input)

    final case class SolT(piecePositions: Array[Int]) {
      override val hashCode: Int = util.Arrays.hashCode(piecePositions) // piecePositions.headOption.getOrElse(-9)

      override def equals(obj: Any): Boolean = obj.isInstanceOf[Array[Int]] &&
        util.Arrays.equals(piecePositions, obj.asInstanceOf[Array[Int]])
    }
    object SolT {
      def apply(solution: Solution): SolT = SolT(solution.toList.toArray.sorted)
    }
    type Solutions = mutable.Set[SolT]
    val seedFactory: Callable[Solutions] = () => new mutable.HashSet[SolT]
    val folder: BiFunction[Solutions, SolT, Solutions] = {
      case (solutions: Solutions, solution: SolT) =>
        assert(solutions.add(solution))
        if (solutions.size % 5000000 == 1)
          print(input, solution.piecePositions)
        solutions
    }

    val solTFlowable: Flowable[SolT] =
      solutionsFlowable
        .buffer(90)
        .observeOn(Schedulers.from(mappingExecutor))
        .flatMap(solutionList => {
          val mappedStream: util.stream.Stream[SolT] = solutionList.stream.map(solution => SolT(solution))
          FlowableUtils.fromJavaIterator(mappedStream.iterator())
        })

    val solutionCount: Long =
      solTFlowable
        .observeOn(Schedulers.single())
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
