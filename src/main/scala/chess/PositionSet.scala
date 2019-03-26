package chess

import chess.model.Position

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet

case class PositionSet(bitSet: BitSet) {

  def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

  def +(position: Position) = PositionSet(bitSet + position.value)

  def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty

  def filter(predicate: Int => Boolean): PositionSet = PositionSet(bitSet.filter(predicate))

  def iterableFrom(minPosition: Position): Iterable[Position] = {
    def bitsIterator: Iterator[Int] = bitSet.iteratorFrom(minPosition.value)

    def iterableWithNewIterator: Iterable[Position] =
      new java.lang.Iterable[Position] {
        override def iterator(): java.util.Iterator[Position] =
          bitsIterator.map(Position(_)).asJava
      }.asScala

    iterableWithNewIterator
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

