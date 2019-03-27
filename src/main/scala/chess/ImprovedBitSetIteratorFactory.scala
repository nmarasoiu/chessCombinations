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
        possibleElem = start,
        iWord = fullWordsCount,
        word = words(fullWordsCount) >>> (start % WordLen)
      )
  }

  class ImprovedBitSetIterator(val words: Array[Long],
                               var possibleElem: Int,
                               var iWord: Int,
                               var word: Long,
                               var initialized: Boolean = false) extends AbstractIterator[Int] {

    @tailrec
    final override def hasNext: Boolean = {
      if ((word & 1L) == 1L) {
        true
      } else if (word == 0L) {
        iWord += 1
        if (iWord < words.length) {
          word = words(iWord)
          possibleElem = iWord * WordLen
          hasNext
        } else {
          false
        }
      } else {
        moveCursor()
        hasNext
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
