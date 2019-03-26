package chess.model

object EncodingConstants {
  val (seven, sevenBits) = (7, 128 - 1)
  val (fourteen, fourteenBits) = (14, 128 * 128 - 1)
}
import EncodingConstants._
case class Position(value: Int) extends Requiring {
  override def requireValueConstraints(): Unit = requireRange(value, minValue = 0, maxValue = fourteenBits)

  //encoding (x,y) as x * horizontal + y as Int
  def x(table: Table): X = X(value % table.horizontal.length)

  def y(table: Table): Y = Y(value / table.horizontal.length)

  def xy(table: Table): XY = XY(x(table), y(table))

  def next(): Position = Position(value + 1)

  def >=(minPosition: Position): Boolean = value >= minPosition.value

}

object Position {
  val zero = Position(0)

  def apply(x: X, y: Y, table: Table): Position = Position(x.value + y.value * table.horizontal.length)
}


case class PositionInTable(value: Int) extends AnyVal {
  def tableAndPosition: (Table, Position) = {
    val lower = value & fourteenBits
    (Table(Horizontal(lower & sevenBits), Vertical(lower >> seven)),
      Position(value >> fourteen))
  }
}

object PositionInTable {
  def apply(position: Position, table: Table): PositionInTable =
    PositionInTable((position.value << fourteen) + (table.vertical.height << seven) + table.horizontal.length)
}
