package chess

import java.util
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable

object ConcurrentSet {

  def createSet[T](): mutable.Set[T] = {
    import scala.collection.JavaConverters._
    createJavaSet[T]().asScala
  }

  private def createJavaSet[T](): util.Set[T] =
    Collections.newSetFromMap(new ConcurrentHashMap[T, java.lang.Boolean])
}

