package chess

import java.{lang, util}

import io.reactivex.Flowable
import io.reactivex.functions.{Function => RxFunction}
import io.reactivex.schedulers.Schedulers

object FlowableUtils {

  implicit class RichFlowable[A](flowable: Flowable[A]) {

    def mapInParallel[B](mapper: A => B): Flowable[B] = mapScala(mapper)(inParallel = true)

    private def mapScala[B](mapper: A => B)(inParallel: Boolean): Flowable[B] =
      flatMapScala(a => Flowable.just(mapper(a)))(inParallel)

    private def flatMapScala[B](mapper: A => Flowable[B])(inParallel: Boolean): Flowable[B] = {
      if (inParallel)
        flowable
          .parallel()
          .runOn(Schedulers.computation())
          .flatMap(asRxFunction(mapper))
          .sequential()
      else
        flowable
          .flatMap(asRxFunction(mapper))
    }

    private def asRxFunction[AA, BB](func: AA => BB): RxFunction[AA, BB] = func(_)

    import scala.collection.JavaConverters._

    def blockingScalaIterable(): Iterable[A] = flowable.blockingIterable().asScala
  }

  import scala.collection.JavaConverters._


  def fromIterable[T](iterable: Iterable[T]): Flowable[T] =
    Flowable.fromIterable(asJava(iterable))

  def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = scalaIterable.asJava

}