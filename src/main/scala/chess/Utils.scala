package chess

import java.{lang, util}

import io.reactivex.Flowable
import io.reactivex.functions.{Function => RxFunction}
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.{Map, SortedSet, TreeSet}

object Utils {
  def equals[T](obj: Any, eqTest: T => Boolean, cls: Class[T]): Boolean = cls.isInstance(obj) && eqTest(obj.asInstanceOf[T])

  def sorted[T](obtainedSet: Set[Set[T]])(implicit o: Ordering[T]): SortedSet[SortedSet[T]] = {
    def _sorted[U](set: Set[U])(implicit o: Ordering[U]): SortedSet[U] = {
      TreeSet[U]() ++ set
    }

    implicit val setOrdering: Ordering[SortedSet[T]] = (x, y) => x.toString.compare(y.toString)
    _sorted(obtainedSet.map(s => _sorted(s)))
  }

  implicit class RichMap[K, V](map: Map[K, V])(implicit cmp: Ordering[K]) {
    def minOption(): Option[(K, V)] = {
      val none: Option[(K, V)] = None
      map.foldLeft(none) {
        case (None, kv) => Some(kv)
        case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
      }
    }
  }

}

object FlowableUtils {

  implicit class RichFlowable[A](inFlow: Flowable[A]) {

    def flatMapInParallel[B]: (A => Flowable[B]) => Flowable[B] = flatMap2(inParallel = true)

    def mapInParallel[B]: (A => B) => Flowable[B] = map2(inParallel = true)

    def flatMap2[B](inParallel: Boolean)(flatMapper: A => Flowable[B]): Flowable[B] = {
      val rxFlatMapper: RxFunction[A, Flowable[B]] = asRxFunction(flatMapper)
      if (inParallel)
        inFlow
          .parallel()
          .runOn(Schedulers.computation())
          .flatMap(rxFlatMapper)
          .sequential()
      else
        inFlow.flatMap(rxFlatMapper)
    }

    def map2[B](inParallel: Boolean)(mapper: A => B): Flowable[B] =
      flatMap2(inParallel)(a => Flowable.just(mapper(a)))
  }

  def asRxFunction[AA, BB](func: AA => BB): RxFunction[AA, BB] = func(_)

  import scala.collection.JavaConverters._

  def fromJavaIterator[T](iterator: util.Iterator[T]): Flowable[T] = {
    fromIterable(iterator.asScala.toIterable)
  }

  def fromIterable[T](iterable: Iterable[T]): Flowable[T] =
    Flowable.fromIterable(asJava(iterable))

  def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = scalaIterable.asJava

}