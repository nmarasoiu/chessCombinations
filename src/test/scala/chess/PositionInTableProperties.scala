package chess

import chess.model._
import org.scalatest.FunSuite

class PositionInTableProperties extends FunSuite {
  def inOrder(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

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
