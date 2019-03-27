package chess

import chess.model._
import org.scalatest.FunSuite

import scala.collection.immutable.BitSet
import scala.util.Random

class PositionInTableProperties extends FunSuite {
  def inOrder(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

  test("BitSet fromIterator") {
    for (len <- Seq(7, 3, 2, 1, 0, 4, 8, 9, 43, 12, 14, 2100, 3200);
         elems = Stream.continually(Random.nextInt(9000)).take(len);
         startIndex <- if (len == 0) Seq(0) else Stream.continually(Random.nextInt(len)).take(12);
         bitSet = BitSet(elems: _*);
         _ <- 1 to 12) {
      import chess.PositionSet.RichBitSet
      val myImplRes = doRepeatedly(bitSet.keysIteratorFromImproved(startIndex).toVector)
      val scalaImplRes = doRepeatedly(bitSet.keysIteratorFrom(startIndex).toVector)
      println("-----")
      assert(myImplRes == scalaImplRes)
    }
  }

  def doRepeatedly[E](expr: => E): E = {
    var i = 0
    val t0 = System.currentTimeMillis()
    while (i < 1) {//set this to like 9999 to see differences; my implementation (inspired from foreach/forall, gets 10 times faster on few elements / many zeroes)
      expr
      i += 1
    }
    val t1 = System.currentTimeMillis()
    println("Time to execute: " + (t1 - t0))
    expr
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
