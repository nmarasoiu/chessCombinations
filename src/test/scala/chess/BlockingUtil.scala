package chess

import java.time.Clock
import java.util.concurrent.Callable

import io.reactivex.functions.BiFunction


object BlockingUtil {
  def executeAndBlock(input: Input, checkDuplication: Boolean = false): Iterable[Solution] = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0nano = System.nanoTime
    val t0 = clock.instant()

    val solutionsStream = GenerationCore.solutions(input)

    import scala.collection.JavaConverters._
    val solutions = solutionsStream.blockingIterable().asScala

    assert(solutions.nonEmpty)
    println(Solution.fromIntToPieceAndCoordinates(solutions.head, input.table))

    if (checkDuplication) {
      case class Solutions(solutionsSoFar: Set[Solution], duplicatedSolutionsSoFar: Set[Solution])
      val seedFactory: Callable[Solutions] = () => Solutions(Set(), Set())
      val folder: BiFunction[Solutions, Solution, Solutions] = {
        case (solutions: Solutions, sol: Solution) =>
          if (solutions.solutionsSoFar(sol))
            Solutions(solutions.solutionsSoFar - sol, solutions.duplicatedSolutionsSoFar + sol)
          else
            Solutions(solutions.solutionsSoFar + sol, solutions.duplicatedSolutionsSoFar)
      }
      val solutions: Solutions = solutionsStream.reduceWith(seedFactory, folder).blockingGet() // blocks
      assert(solutions.duplicatedSolutionsSoFar.isEmpty)
    }
    val solutionsCount = solutions.size // blocks
    val t1 = clock.instant()
    val t1nano = System.nanoTime
    println(" computed in " + java.time.Duration.between(t0, t1) + " / " +
      ((t1nano.toDouble - t0nano) / 1000D / 1000 / 1000) +
    " -> " + solutionsCount + " solutions found")

    solutions
  }

}
