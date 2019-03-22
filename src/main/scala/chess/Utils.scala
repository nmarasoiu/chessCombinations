package chess

import java.{lang, util}

import io.reactivex.Flowable
import io.reactivex.functions.{Function => RxFunction}
import io.reactivex.schedulers.Schedulers

import scala.collection.immutable.{Map, SortedSet, TreeSet}

object Utils {
  def sorted[T](obtainedSet: Set[Set[T]])(implicit o: Ordering[T]): SortedSet[SortedSet[T]] = {
    def sorted[U](set: Set[U])(implicit o: Ordering[U]): SortedSet[U] = TreeSet[U]() ++ set

    implicit val setOrdering: Ordering[SortedSet[T]] = (x, y) => x.toString.compare(y.toString)
    sorted(obtainedSet.map(s => sorted(s)))
  }

  implicit class RichBitSet(bitSet: Positions) {
    def intersects(other: Positions): Boolean = (bitSet & other).nonEmpty
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
  implicit class RichFlowable[A](flowable: Flowable[A]) {
    def mapInParallel[B](mapper: A => B): Flowable[B] =
      flatMapInParallel(a => Flowable.just(mapper(a)))

    def flatMapInParallel[B](flatMapper: A => Flowable[B]): Flowable[B] = {
      def asRxFunction[AA, BB](func: AA => BB): RxFunction[AA, BB] = func(_)
      val rxFlatMapper: RxFunction[A, Flowable[B]] = asRxFunction(flatMapper)
      flowable
        .parallel()
        .runOn(Schedulers.computation())
        .flatMap(rxFlatMapper)
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