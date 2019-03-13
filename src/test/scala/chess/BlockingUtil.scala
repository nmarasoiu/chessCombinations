package chess

import java.time.Clock
import java.util.concurrent.Callable

import io.reactivex.functions.BiFunction


object BlockingUtil {
  def blockingIterable(input: Input): Iterable[Solution] = FlowableUtils.toIterable(GenerationCore.solutions(input))

  def blockingTest(input: Input, checkDuplication: Boolean = false): Long = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0nano = System.nanoTime
    val t0 = clock.instant()

    val solutionsFlowable = GenerationCore.solutions(input)


    case class Solutions(solutionsSoFar: Set[Solution], duplicatedSolutionsSoFar: Set[Solution])
    val seedFactory: Callable[Solutions] = () => Solutions(Set(), Set())
    val folder: BiFunction[Solutions, Solution, Solutions] = {
      case (solutions: Solutions, solution: Solution) =>
        if (solutions.solutionsSoFar.size % 1000 == 1)
          print(input, solution)
        if (solutions.solutionsSoFar(solution))
          Solutions(solutions.solutionsSoFar, solutions.duplicatedSolutionsSoFar + solution)
        else
          Solutions(solutions.solutionsSoFar + solution, solutions.duplicatedSolutionsSoFar)
    }
    val solutions: Solutions = solutionsFlowable.reduceWith(seedFactory, folder).blockingGet() // blocks
    assert(solutions.duplicatedSolutionsSoFar.isEmpty)
    val solutionCount: Long = solutions.solutionsSoFar.size

    val t1 = clock.instant()
    val t1nano = System.nanoTime
    println(" computed in " + java.time.Duration.between(t0, t1) + " / " +
      ((t1nano.toDouble - t0nano) / 1000D / 1000 / 1000) + " -> " + solutionCount + " solutionFlowable found")

    solutionCount
  }

  private def print(input: Input, solution: Solution): Unit = {
    println(Solution.fromIntToPieceAndCoordinates(solution, input.table))
  }
}
