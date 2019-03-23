package chess

import java.{lang, util}

import io.reactivex.Flowable
import io.reactivex.functions.{Function => RxFunction}
import io.reactivex.schedulers.Schedulers

object FlowableUtils {

  implicit class RichFlowable[A](flowable: Flowable[A]) {

    def mapInParallel[B](mapper: A => B): Flowable[B] =
      flatMapInParallel(a => Flowable.just(mapper(a)))

    def asRxFunction[AA, BB](func: AA => BB): RxFunction[AA, BB] = func(_)

    def flatMapScala[B](flatMapper: A => Flowable[B]): Flowable[B] =
      flowable.flatMap(asRxFunction(flatMapper))

    def flatMapInParallel[B](flatMapper: A => Flowable[B]): Flowable[B] = {
      flowable
        .parallel()
        .runOn(Schedulers.computation())
        .flatMap(asRxFunction(flatMapper))
        .sequential()
    }

    import scala.collection.JavaConverters._

    def blockingScalaIterable(): Iterable[A] = flowable.blockingIterable().asScala
  }

  import scala.collection.JavaConverters._

  def fromJavaIterator[T](iterator: util.Iterator[T]): Flowable[T] = {
    fromIterable(iterator.asScala.toIterable)
  }

  def fromIterable[T](iterable: Iterable[T]): Flowable[T] =
    Flowable.fromIterable(asJava(iterable))

  def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = scalaIterable.asJava

}