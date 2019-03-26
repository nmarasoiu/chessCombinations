package chess.extensions

import java.{lang, util}
import scala.collection.JavaConverters._
import scala.collection.immutable.{SortedMap, TreeMap}

object CollectionsExtensions {

  object IteratorFactoryBasedIterable {
    def apply[A](iteratorFactory: () => Iterator[A]): Iterable[A] =
      new IteratorFactoryBasedIterable(iteratorFactory).asScala
  }

  private class IteratorFactoryBasedIterable[A](val iteratorFactory: () => Iterator[A]) extends lang.Iterable[A] {
    override def iterator(): util.Iterator[A] = iteratorFactory.apply().asJava
  }

  implicit class RichMap[K, V](map: Map[K, V])(implicit ord: Ordering[K]) {

    def toSortedMap: SortedMap[K, V] = TreeMap[K, V]() ++ map

    def minOption(): Option[(K, V)] = {
      if (map.isInstanceOf[SortedMap[K, V]]) {
        map.headOption
      } else if (map.isEmpty) {
        None
      } else {
        Some(min)
      }
    }

    def min: (K, V) = {
      def reducer(kv1: (K, V), kv2: (K, V)): (K, V) = {
        if (ord.lt(kv1._1, kv2._1)) kv1 else kv2
      }

      map.reduce(reducer)
    }
  }

}
