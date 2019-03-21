package chess

import java.{lang, util}

import io.reactivex.Flowable
import io.reactivex.functions.Function

import scala.collection.immutable.{Map, SortedSet, TreeSet}

object Utils {

  def sorted[T](obtainedSet: Set[Set[T]])(implicit o: Ordering[T]): SortedSet[SortedSet[T]] = {
    def _sorted[U](set: Set[U])(implicit o: Ordering[U]): SortedSet[U] = {
      TreeSet[U]() ++ set
    }

    implicit val setOrdering: Ordering[SortedSet[T]] = (x, y) => x.toString.compare(y.toString)
    _sorted(obtainedSet.map(s => _sorted(s)))
  }

  def minOptional[K, V](map: Map[K, V])(implicit cmp: Ordering[K]): Option[(K, V)] = {
    val none: Option[(K, V)] = None
    map.foldLeft(none) {
      case (None, kv) => Some(kv)
      case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
    }
  }

}

object FlowableUtils {

  def asRxFunction[T](func: Position => Flowable[T]): Function[Integer, Flowable[T]] = {
    position: Integer => func(position)
  }

  import scala.collection.JavaConverters._

  def fromJavaIterator[T](iterator: util.Iterator[T]): Flowable[T] = {
    fromIterable(iterator.asScala.toIterable)
  }

  def fromIterable[T](iterable: Iterable[T]): Flowable[T] =
    Flowable.fromIterable(asJava(iterable))

  private def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = scalaIterable.asJava

  def blockToIterable[T](flowable: Flowable[T]): Iterable[T] = //todo move in test
    asScala(flowable.blockingIterable())

  private def asScala[T](javaIterable: lang.Iterable[T]): Iterable[T] = javaIterable.asScala
}