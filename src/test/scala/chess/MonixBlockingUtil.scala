package chess

import java.time.Clock

import monix.eval.Task
import monix.execution.CancelableFuture
import monix.reactive.Observable
import monix.execution.Scheduler.{Implicits => monixImplicits}


object MonixBlockingUtil {
  //todo this is in fact keeping all the potential solutions in memory; to implement in truly streaming mode, emitting as it goes & letting go of emitted solutions
  //still the current implementation is streaming if the processing e.g. printing the solutions is done in foreach on the observable
  def block(o: Observable[PotentialSolution]): Iterable[PotentialSolution] = {
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import monixImplicits.global

    val task = Task.fork(o.toListL)
    val future: CancelableFuture[List[PotentialSolution]] = task.runAsync

    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val solutions: Iterable[PotentialSolution] = Await.result(future, Duration.Inf)

    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + solutions.size + " solutions found")
    // yes size is O(n) not O(1) but the real streaming on printing/processing and/or the memory freeup as elements are emitted+processed and then freed
    //ok seems we need to go implementing Reactive Publisher and not use toListL, if we want to really stream and have memory collectible for processed elements/solutions/boards
    solutions
  }

}
