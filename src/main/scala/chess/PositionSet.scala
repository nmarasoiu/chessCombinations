package chess

import java.{lang, util}

import chess.extensions.CollectionsExtensions.IteratorFactoryBasedIterable
import chess.model.Position

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet

case class PositionSet(bitSet: BitSet) {

  def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

  def +(position: Position) = PositionSet(bitSet + position.value)

  def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty


  def iterableFrom(minPosition: Position): Iterable[Position] = {
    IteratorFactoryBasedIterable(
      () => bitSet.iteratorFrom(minPosition.value))
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

