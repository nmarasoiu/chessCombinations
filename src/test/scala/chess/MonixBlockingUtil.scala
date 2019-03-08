package chess

import java.time.Clock

import monix.eval.Task
import monix.execution.CancelableFuture
import monix.reactive.Observable
import monix.execution.Scheduler.{Implicits => monixImplicits}


object MonixBlockingUtil {
  //todo this is in fact keeping all the potential solutions in memory; to implement in truly streaming mode, emitting as it goes & letting go of emitted solutions
  //still the current implementation is streaming if the processing e.g. printing the solutions is done in foreach on the observable
  def block(stream: Observable[PotentialSolution], checkDuplication:Boolean=true): Iterable[PotentialSolution] = {
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import monixImplicits.global

    val task = Task.fork(stream.toListL)
    val future: CancelableFuture[List[PotentialSolution]] = task.runAsync

    val clock = Clock.systemUTC()
    val t0 = clock.instant()

    val solutions: IndexedSeq[PotentialSolution] = Await.result(future, Duration.Inf).toIndexedSeq

    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + solutions.size + " solutions found")

    assert(solutions.nonEmpty)
    Seq(solutions.head,solutions.last).distinct.foreach(solution =>{
      println(solution)
    })
    if(checkDuplication) {
      assert(solutions.distinct.length == solutions.size) //todo: should we try some .par ? a single core is used quite a lot of time for distinct which creates a set
    }
    solutions
  }

}
