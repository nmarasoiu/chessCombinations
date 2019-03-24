package chess

import org.scalatest.FunSuite

class PositionInTableProperties extends FunSuite {
  def inOrder(a: Int, b: Int, c: Int): Boolean = a <= b && b <= c

  test("Table should serialize to int and deserialize back the same") {
    for (positionInt: Int <- 0 to 130;
         horizontalInt: Int <- 1 to 17;
         verticalInt: Int <- 1 to 127) {

      val horizontal = Horizontal(horizontalInt)
      val vertical = Vertical(verticalInt)
      val table = chess.Table(horizontal, vertical)
      val position = Position(positionInt)
      val positionInTable = PositionInTable(position, table)
      val (resultTable, resultPosition) = positionInTable.tableAndPosition

      assert(resultTable == table, s"Table does not deserialize the same: got $resultTable but should have been $table")
      assert(resultPosition == position, s"Position does not deserialize the same: got $resultPosition" +
        s" but should have been $position")
    }
  }
}
