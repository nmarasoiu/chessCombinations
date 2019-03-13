package chess

import java.time.Clock

import io.reactivex.Flowable


object BlockingUtil {
  def block(stream: Flowable[PotentialSolution], checkDuplication: Boolean): Iterable[PotentialSolution] = {
    val clock = Clock.systemDefaultZone()
    val t0 = clock.instant()
    import scala.collection.JavaConverters._
    val solutions = stream.blockingIterable().asScala
    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + solutions.size + " solutions found")

    assert(solutions.nonEmpty)
    Seq(solutions.head, solutions.last).distinct.foreach(solution => {
      println(solution)
    })
    if (checkDuplication) {
//      assert(solutions.distinct.length == solutions.size) //todo: should we try some .par ? a single core is used quite a lot of time for distinct which creates a set
    }
    solutions
  }

}
