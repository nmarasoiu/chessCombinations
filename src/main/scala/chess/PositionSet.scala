package chess

import chess.CollectionEnrichments.RichIterator
import chess.model.Position

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet

case class PositionSet(bitSet: BitSet) {

  def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

  def +(position: Position) = PositionSet(bitSet + position.value)

  def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty

  def filter(predicate: Int => Boolean): PositionSet = PositionSet(bitSet.filter(predicate))

  def iterableFrom(minPosition: Position): Iterable[Position] = {
    RichIterator[Int](_ => bitSet
      .iteratorFrom(minPosition.value)
    ).toResubscribeIterable
      .map(Position(_))
  }
}

object PositionSet {
  val empty = PositionSet(BitSet())

  def apply(): PositionSet = empty

  def apply(positions: Seq[Int]): PositionSet = PositionSet(BitSet(positions: _*))
}

object PositionSet2 {
  def apply(positions: Seq[Position]): PositionSet = PositionSet(positions.map(_.value))
}

object CollectionEnrichments {

  case class RichIterator[A](originalIteratorFactory: Unit => Iterator[A]) {
    lazy val toResubscribeIterable: Iterable[A] =
      new java.lang.Iterable[A] {
        override def iterator(): java.util.Iterator[A] = {
          originalIteratorFactory.apply().asJava
        }
      }.asScala
  }

}
