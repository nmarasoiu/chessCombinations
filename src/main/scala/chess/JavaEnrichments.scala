package chess
import scala.collection.JavaConverters._

object JavaEnrichments {
  implicit class RichScalaIterator[A](scalaIterator: Iterator[A]) {
    def toJavaIterable: Iterable[A] =
      new java.lang.Iterable[A] {
        override def iterator(): java.util.Iterator[A] = {
          scalaIterator.asJava
        }
      }.asScala
  }
}
