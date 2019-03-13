package chess

import java.lang

import io.reactivex.Flowable

import scala.collection.immutable.Map

object Utils {
  def minOptional[K, V](map: Map[K, V])(implicit cmp: Ordering[K]): Option[(K, V)] = {
    val none: Option[(K, V)] = None
    map.foldLeft(none) {
      case (None, kv) => Some(kv)
      case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
    }
  }

}

object FlowableUtils {

  def fromIterable[T](iterable: Iterable[T]): Flowable[T] =
    Flowable.fromIterable(asJava(iterable))

  private def asJava[T](scalaIterable: Iterable[T]): lang.Iterable[T] = scalaIterable.asJava

  import scala.collection.JavaConverters._

  def toIterable[T](flowable: Flowable[T]): Iterable[T] =
    asScala(flowable.blockingIterable())

  private def asScala[T](javaIterable: lang.Iterable[T]): Iterable[T] = javaIterable.asScala
}