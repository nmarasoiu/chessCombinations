package chess

import java.time.Clock

import io.reactivex.Flowable


object BlockingUtil {
  def block(stream: Flowable[PotentialSolution], checkDuplication: Boolean): Iterable[PotentialSolution] = {
    println("Computing..")
    val clock = Clock.systemUTC()
    val t0nano = System.nanoTime
    val t0 = clock.instant()
    import scala.collection.JavaConverters._
    val solutions = stream.blockingIterable().asScala
    assert(solutions.nonEmpty)
    Seq(solutions.head, solutions.last).distinct.foreach(solution => {
      println(solution)
    })
    if (checkDuplication) {
      //todo only extract some duplicates if any with bloom filter or other LogLog like probabilistic dup detector
      assert(stream.distinct.blockingIterable().asScala.size == solutions.size) //todo: should we try some .par ? a single core is used quite a lot of time for distinct which creates a set
    }
    val t1 = clock.instant()
    val t1nano = System.nanoTime
    println(" computed in " + java.time.Duration.between(t0, t1) + " / " +
      ((t1nano.toDouble - t0nano) / 1000D / 1000 / 1000) +
      " -> " + solutions.size + " solutions found")
    solutions
  }

}
