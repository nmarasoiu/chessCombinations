package chess

import chess.model._
import org.scalatest.FunSuite

import scala.collection.immutable.BitSet
import scala.util.Random

class PositionInTableProperties extends FunSuite {
  def inOrder(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

  test("BitSet fromIterator") {
    for (maxElement <- 0 until Int.MaxValue;
         randoms = Stream.continually(Random.nextInt(1 + maxElement));
         takeCount <- 0 to maxElement;
         elements = randoms.take(takeCount);
         startIndex <- if (maxElement == 0) Seq(0) else randoms.take(takeCount);
         bitSet = BitSet(elements: _*)) {
      import chess.PositionSet.RichBitSet
      val (myImplRes, scalaImplRes) = (
        bitSet.iteratorFromImproved(startIndex).toIndexedSeq,
        bitSet.from(startIndex).toIndexedSeq)
      assert(myImplRes == scalaImplRes)
    }
  }

  def time[E](expr: => E): E = {
    var evaluated: Option[E] = None
    val t0 = System.currentTimeMillis()
    evaluated = Some(expr)
    val t1 = System.currentTimeMillis()
    println("Time to execute: " + (t1 - t0))
    evaluated.get
  }

  test("Table should serialize to int and deserialize back the same") {
    for (positionInt: Int <- Seq(0, 3, 4, 5, 12, 127, 128, 130);
         horizontalInt: Int <- Seq(2, 1, 3, 4, 5, 12, 127);
         verticalInt: Int <- Seq(1, 2, 3, 4, 5, 12, 127)) {

      val horizontal = Horizontal(horizontalInt)
      val vertical = Vertical(verticalInt)
      val table = Table(horizontal, vertical)
      val position = Position(positionInt)
      val positionInTable = PositionInTable(position, table)
      val (resultTable, resultPosition) = positionInTable.tableAndPosition

      assert(resultTable == table, s"Table does not deserialize the same: got $resultTable but should have been $table")
      assert(resultPosition == position, s"Position does not deserialize the same: got $resultPosition" +
        s" but should have been $position")
    }
  }
}
