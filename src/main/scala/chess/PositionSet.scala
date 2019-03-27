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
    val words: Array[Long] = bitSet.toBitMask
    val len: Int = words.length

    def iteratorFromImproved(start: Int): Iterator[Int] = keysIteratorFromImproved(start)

    def keysIteratorFromImproved(start: Int): Iterator[Int] = new AbstractIterator[Int] {
      private var possibleElem: Int = start
      private final val WordLen = 64
      private var iWord: Int = start / WordLen
      private var wordInitialized: Boolean = false
      private var word: Long = 0

      @tailrec
      override final def hasNext: Boolean = {
        if (iWord >= len) {
          false
        } else {
          if (!wordInitialized) {
            word = words(iWord) >>> (start % WordLen)
            wordInitialized = true
          }
          if (word == 0L) {
            iWord += 1
            if (iWord < len) {
              word = words(iWord)
              possibleElem = iWord * WordLen
            }
            hasNext
          } else if ((word & 1L) == 1L) {
            true
          } else {
            moveCursor()
            hasNext
          }
        }
      }

      override def next(): Int = {
        val elem = possibleElem
        moveCursor()
        elem
      }

      def moveCursor(): Unit = {
        assert(word != 0L)
        word = word >>> 1
        possibleElem += 1
      }
    }


  }

  val empty = PositionSet(BitSet())

  def apply(): PositionSet = empty

  def apply(positions: Seq[Int]): PositionSet = PositionSet(BitSet(positions: _*))
}

object PositionSet2 {
  def apply(positions: Seq[Position]): PositionSet = PositionSet(positions.map(_.value))
}

