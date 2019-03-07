import chess._
import monix.eval.Task
import monix.execution.Scheduler.{Implicits => monixImplicits}
import monix.reactive.Observable

object MonixBlockingUtil {

  def block(o: Observable[PotentialSolution]): Iterable[PotentialSolution] = {
    import monixImplicits.global

    import scala.concurrent.Await
    import scala.concurrent.duration._

    val task = Task.fork(o.toListL)
    val future = task.runAsync
    Await.result(future, Duration.Inf)
  }

}
