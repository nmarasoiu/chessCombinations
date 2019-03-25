package chess

object Enrichments {

  implicit class RichMap[K, V](map: Map[K, V]) {
    val none: Option[(K, V)] = None

    def minOption(implicit ord: Ordering[K]): Option[(K, V)] = {
      if (map.isEmpty) {
        None
      } else {
        Some(min(map))
      }
    }

    def min(map: Map[K, V])(implicit ord: Ordering[K]): (K, V) = {
      def reducer(kv1: (K, V), kv2: (K, V)): (K, V) = {
        if (ord.lt(kv1._1, kv2._1)) kv1 else kv2
      }

      map.reduce(reducer)
    }
  }

}
