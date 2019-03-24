

package object chess {

  import chess.Piece

  import scala.collection.immutable.BitSet

  val (three, threeBits) = (3, 7)
  val (seven, sevenBits) = (7, 128 - 1)
  val (fourteen, fourteenBits) = (14, 128 * 128 - 1)

  //todo try require() in constructor/trait instead of assert
  def assertRange[T](value: Int, minValue: Int, maxValue: Int, cls: Class[T]): Unit =
    assert(minValue <= value && value <= maxValue,
      s"Condition $minValue <= $value <= $maxValue not respected for $cls")

  case class XOffset(xOffsetInt: Int) {
    def unary_- = XOffset(-xOffsetInt)
  }

  object XOffset {
    val zero: XOffset = XOffset(0)
    val one: XOffset = XOffset(1)
  }

  case class YOffset(yOffsetInt: Int) {
    def unary_- = YOffset(-yOffsetInt)
  }

  object YOffset {
    val zero: YOffset = YOffset(0)
    val one: YOffset = YOffset(1)
  }

  //todo remove duplication between X and Y with type programming w.g. abstract member class
  // & see with assertRange
  case class X(x: Int) {
//    assertRange(x, minValue = 0, maxValue = sevenBits, classOf[X])

    def +(len: XOffset): X = X(x + len.xOffsetInt)

    def -(len: XOffset): X = X(x - len.xOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= x && x < table.horizontal.length
  }



  case class Y(y: Int) {
//    assertRange(y, minValue = 0, maxValue = sevenBits, classOf[Y])

    def +(len: YOffset): Y = Y(y + len.yOffsetInt)

    def -(len: YOffset): Y = Y(y - len.yOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= y && y < table.vertical.height
  }

  case class XY(x: X, y: Y) {
    def fitsIn(table: Table): Boolean = x.fitsIn(table) && y.fitsIn(table)
  }

  case class Horizontal(length: Int) {
    assertRange(length, minValue = 1, maxValue = sevenBits, classOf[Horizontal])
  }

  case class Vertical(height: Int) {
    assertRange(height, minValue = 1, maxValue = sevenBits, classOf[Vertical])
  }

  case class Count(count: Int) {
    assertRange(count, minValue = 1, maxValue = Int.MaxValue, classOf[Count])

    def decremented(): Count = Count(count - 1)
  }

  object Count {
    val one = Count(1)
  }

  case class Position(positionInt: Int) {
    assertRange(positionInt, 0, fourteenBits, classOf[Position])

    //encoding (x,y) as x*horiz+y as Int
    def x(table: Table): X = X(positionInt % table.horizontal.length)

    def y(table: Table): Y = Y(positionInt / table.horizontal.length)

    def xy(table: Table): XY = XY(x(table), y(table))

    def next(): Position = Position(positionInt + 1)

    def >=(minPosition: Position): Boolean = positionInt >= minPosition.positionInt
  }

  object Position {
    val zero = Position(0)

    def apply(x: X, y: Y, table: Table): Position = Position(x.x + y.y * table.horizontal.length)
  }

  case class Table(horizontal: Horizontal, vertical: Vertical) {
    def area: Int = vertical.height * horizontal.length
  }

  case class PositionInTable(pit: Int) extends AnyVal {
    def tableAndPosition: (Table, Position) = {
      val lower = pit & fourteenBits
      (Table(Horizontal(lower & sevenBits), Vertical(lower >> seven)),
        Position(pit >> fourteen))
    }
  }

  object PositionInTable {
    def apply(position: Position, table: Table): PositionInTable =
      PositionInTable((position.positionInt << fourteen) + (table.vertical.height << seven) + table.horizontal.length)
  }

  case class Pick(piece: Piece, position: Position) {
  }

  case class PositionSet(bitSet: BitSet) {
    //encoding (x,y) as x*horiz+y as Int
    def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

    def +(position: Position) = PositionSet(bitSet + position.positionInt)

    def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty

    def filter(predicate: Int => Boolean): PositionSet = PositionSet(bitSet.filter(predicate))

    def iteratorFrom(minPosition: Position): Iterator[Position] =
      bitSet.iteratorFrom(minPosition.positionInt).map(Position(_))
  }

  object PositionSet {
    def apply(): PositionSet = PositionSet(BitSet())

    def apply(positions: Seq[Int]): PositionSet = PositionSet(BitSet(positions: _*))
  }

  object PositionSet2 {
    def apply(positions: Seq[Position]): PositionSet = PositionSet(positions.map(_.positionInt))
  }

  case class SubSolution(picks: List[Pick]) {
    def +(pick: Pick): SubSolution = SubSolution(pick :: picks)
  }

  object SubSolution {
    val Empty = SubSolution(Nil)
    def apply(): SubSolution = Empty
  }

}