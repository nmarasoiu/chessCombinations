package chess

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.{AbstractIterator, Iterator}

object ImprovedBitSetIteratorFactory {
  private final val WordLen = 64

  def iterator(bitSet: BitSet, start: Int): Iterator[Int] = {
    val words: Array[Long] = bitSet.toBitMask
    val fullWordsCount = start / WordLen
    if (fullWordsCount >= words.length)
      Iterator.empty
    else
      new ImprovedBitSetIterator(
        words = bitSet.toBitMask,
        nextPotentialElement = start,
        iWord = fullWordsCount,
        word = words(fullWordsCount) >>> (start % WordLen)
      )
  }

  class ImprovedBitSetIterator(val words: Array[Long],
                               var nextPotentialElement: Int,
                               var iWord: Int,
                               var word: Long) extends AbstractIterator[Int] {
    @tailrec
    override final def hasNext: Boolean = {
      (word & 1L) == 1L || {
        if (word == 0L) {
          iWord += 1
          if (iWord < words.length) {
            word = words(iWord)
            nextPotentialElement = iWord * WordLen
            hasNext
          } else {
            false
          }
        } else {
          moveBitCursor()
          hasNext
        }
      }
    }

    override def next(): Int = {
      val elem = nextPotentialElement
      moveBitCursor()
      elem
    }

    private final def moveBitCursor() {
      assert(word != 0L)//todo remove
      word = word >>> 1
      nextPotentialElement += 1
    }
  }

}
