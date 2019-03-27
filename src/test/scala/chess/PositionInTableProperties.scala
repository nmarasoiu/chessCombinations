package chess

import chess.model._
import org.scalatest.FunSuite

import scala.collection.immutable.BitSet
import scala.util.Random

class PositionInTableProperties extends FunSuite {
  def inOrder(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

  test("BitSet fromIterator") {
    for (len <- 0 to 9000;
         maxElement = if (len == 0) 0 else len;
         takeCount = 1 + maxElement / 12;
         elements = Stream.continually(Random.nextInt(1 + maxElement)).take(takeCount);
         startIndex <- if (len == 0) Seq(0) else Stream.continually(Random.nextInt(len)).take(takeCount);
         bitSet = BitSet(elements: _*)) {
      import chess.PositionSet.RichBitSet
      val myImplRes = time(bitSet.keysIteratorFromImproved(startIndex).toIndexedSeq)
      val scalaImplRes = time(bitSet.from(startIndex).toIndexedSeq)
      println(s"--$len---")
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
