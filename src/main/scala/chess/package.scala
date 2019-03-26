

//todo split this huge object
package object chess {

  val (three, threeBits) = (3, 7)
  val (seven, sevenBits) = (7, 128 - 1)
  val (fourteen, fourteenBits) = (14, 128 * 128 - 1)

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

  abstract class Requiring {

    def requireValueConstraints(): Unit

    def requirePositiveUpToSevenBits(minValue: Int = 0): Unit = requireRange(value, minValue, maxValue = sevenBits)

    def requireSevenBitsPlusMinusEvenBits(): Unit = requireRange(value, minValue = -sevenBits, maxValue = 2 * sevenBits)

    def value: Int

    def requireRange[T](value: Int, minValue: Int, maxValue: Int): Unit =
      require(minValue <= value && value <= maxValue,
        s"Condition $minValue <= $value <= $maxValue not respected for $getClass")
  }

  abstract class RequiringSevenBitsLenient extends Requiring {
  }

  abstract class LenientX extends RequiringSevenBitsLenient {
    def toX: X = X(value)

    def +(len: XOffset) = AnyX(value + len.xOffsetInt)

    def -(len: XOffset) = AnyX(value - len.xOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= value && value < table.horizontal.length
  }

  case class AnyX(override val value: Int) extends LenientX {

    override def requireValueConstraints(): Unit = requireSevenBitsPlusMinusEvenBits()
  }

  case class X(override val value: Int) extends LenientX {
    override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits()
  }

  abstract class LenientY extends RequiringSevenBitsLenient {
    def toY: Y = Y(value)

    def +(len: YOffset) = AnyY(value + len.yOffsetInt)

    def -(len: YOffset) = AnyY(value - len.yOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= value && value < table.vertical.height
  }

  case class AnyY(override val value: Int) extends LenientY {

    override def requireValueConstraints(): Unit = requireSevenBitsPlusMinusEvenBits()
  }

  case class Y(override val value: Int) extends LenientY {
    override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits()
  }

  case class XY(x: X, y: Y) {
  }

  case class AnyXY(x: AnyX, y: AnyY) {
    def fitsIn(table: Table): Boolean = x.fitsIn(table) && y.fitsIn(table)

    def toXY: XY = XY(x.toX, y.toY)
  }

  case class Horizontal(length: Int) extends Requiring {
    override def value: Int = length

    override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits(minValue = 1)
  }

  case class Vertical(height: Int) extends Requiring {
    override def value: Int = height

    override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits(minValue = 1)
  }

  case class Count(value: Int) extends Requiring {
    override def requireValueConstraints(): Unit = requireRange(value, minValue = 1, maxValue = Int.MaxValue)

    def decremented(): Count = Count(value - 1)
  }

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

  case class Table(horizontal: Horizontal, vertical: Vertical) {
    def area: Int = vertical.height * horizontal.length
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


  case class Pick(piece: Piece, position: Position) extends Ordered[Pick] {
    override def compare(that: Pick): Int = PickOrdering.compare(this, that)
  }

  object PickOrdering extends Ordering[Pick] {
    override def compare(x: Pick, y: Pick): Int = {
      val firstCompared = x.piece.compare(y.piece)
      if (firstCompared != 0) {
        firstCompared
      } else {
        x.position.value - y.position.value
      }
    }
  }


  case class SubSolution(picks: List[Pick]) {
    def +(pick: Pick): SubSolution = SubSolution(pick :: picks)

    def +(piecePosition: (Piece, Position)): SubSolution = piecePosition match {
      case (piece, position) => this.+(Pick(piece, position))
    }
  }

  object SubSolution {
    val Empty = SubSolution(Nil)

    def apply(): SubSolution = Empty
  }

}