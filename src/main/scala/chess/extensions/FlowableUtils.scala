package chess.extensions

import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable
import io.reactivex.schedulers.Schedulers
import scala.collection.JavaConverters._

object FlowableUtils {

  def fromIterable[T](iterable: Iterable[T]): Flowable[T] = Flowable.fromIterable(iterable.asJava)

  implicit class RichFlowable[A](flowable: Flowable[A]) {

    def mapInParallel[B](mapper: A => B): Flowable[B] = mapScala(mapper)(inParallel = true)

    private def mapScala[B](mapper: A => B)(inParallel: Boolean): Flowable[B] =
      flatMapScala(a => Flowable.just(mapper(a)))(inParallel)

    def parallelOnComputation(): ParallelFlowable[A] = flowable.parallel().runOn(Schedulers.computation())

    private def flatMapScala[B](mapper: A => Flowable[B])(inParallel: Boolean): Flowable[B] = {
      if (inParallel)
        flowable
          .parallelOnComputation()
          .flatMap(asRxFunction(mapper))
          .sequential()
      else
        flowable
          .flatMap(asRxFunction(mapper))
    }

    import io.reactivex.functions.{Function=>RxFunction}
    def asRxFunction[AA, BB](func: AA => BB): RxFunction[AA, BB] = func(_)

    def blockingScalaIterable(): Iterable[A] = flowable.blockingIterable().asScala
  }
}
