package chess

import scala.collection.immutable.{SortedMap, TreeMap}
import scala.collection.AbstractIterable

object Enrichments {

  implicit class RichMap[K, V](map: Map[K, V])(implicit ord: Ordering[K]) {

    def toSortedMap: SortedMap[K,V] = TreeMap[K,V]() ++ map

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

  implicit class RichIterator[A](iteratorA: Iterator[A]) {
    def toOneTimeIterable: Iterable[A] = new AbstractIterable[A](){
      override def iterator: Iterator[A] = iteratorA
    }
  }

}
