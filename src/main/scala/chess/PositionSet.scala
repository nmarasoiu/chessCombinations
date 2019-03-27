package chess

import chess.extensions.CollectionsExtensions.IteratorFactoryBasedIterable
import chess.model.Position

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.{AbstractIterator, Iterator}

case class PositionSet(bitSet: BitSet) {

  def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

  def +(position: Position) = PositionSet(bitSet + position.value)

  def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty

  def iterableFrom(minPosition: Position): Iterable[Position] = {
    IteratorFactoryBasedIterable(() => iteratorFrom(minPosition))
  }

  def iteratorFrom(minPosition: Position): Iterator[Position] = {
    import chess.PositionSet.RichBitSet
    bitSet.iteratorFromImproved(minPosition.value).map(Position(_))
  }

}

object PositionSet {

  implicit final class RichBitSet(bitSet: BitSet) {

    def iteratorImproved(): Iterator[Int] = iteratorFromImproved(0)

    def iteratorFromImproved(start: Int): Iterator[Int] = keysIteratorFromImproved(start)

    def keysIteratorFromImproved(start: Int): Iterator[Int] = ImprovedBitSetIteratorFactory.iterator(bitSet, start)
}

val empty = PositionSet (BitSet () )

def apply (): PositionSet = empty

def apply (positions: Seq[Int] ): PositionSet = PositionSet (BitSet (positions: _*) )
}

object PositionSet2 {
  def apply(positions: Seq[Position]): PositionSet = PositionSet(positions.map(_.value))
}

