import scala.collection.immutable.BitSet

package object chess {

  /**
    * An Int wil have 32 bits:
    * - sign 1 bit
    * - piece 3 bits
    * - position: 2 * 7 bits
    * - table: 2 * 7 bits
    */
  val (three, threeBits) = (3, 7)
  val (seven, sevenBits) = (7, 128 - 1)
  val (fourteen, fourteenBits) = (14, 128 * 128 - 1)

  def assertRange(value: Int, minValue: Int, maxValue: Int): Boolean =
    minValue <= value && value <= maxValue

  final case class XOffset(xOffsetInt: Int) {
    def unary_- = XOffset(-xOffsetInt)
  }

  object XOffset {
    val zero: XOffset = XOffset(0)
    val one: XOffset = XOffset(1)
  }

  final case class YOffset(yOffsetInt: Int) {
    def unary_- = YOffset(-yOffsetInt)
  }

  object YOffset {
    val zero: YOffset = YOffset(0)
    val one: YOffset = YOffset(1)
  }

  final case class X(x: Int) {
    assertRange(x, minValue = 0, maxValue = sevenBits)

    def +(len: XOffset): X = X(x + len.xOffsetInt)

    def -(len: XOffset): X = X(x - len.xOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= x && x < table.horizontal.length
  }

  final case class Y(y: Int) {
    assertRange(y, minValue = 0, maxValue = sevenBits)

    def +(len: YOffset): Y = Y(y + len.yOffsetInt)

    def -(len: YOffset): Y = Y(y - len.yOffsetInt)

    def fitsIn(table: Table): Boolean = 0 <= y && y < table.vertical.height
  }

  final case class XY(x: X, y: Y) {
    def fitsIn(table: Table): Boolean = x.fitsIn(table) && y.fitsIn(table)
  }

  final case class Horizontal(length: Int) {
    assertRange(length, minValue = 1, maxValue = sevenBits)
  }

  final case class Vertical(height: Int) {
    assertRange(height, minValue = 1, maxValue = sevenBits)
  }

  final case class Count(count: Int) {
    assertRange(count, minValue = 1, maxValue = sevenBits)

    def decremented(): Count = Count(count - 1)
  }

  final case class Position(positionInt: Int) {
    def >=(minPosition: Position): Boolean = positionInt >= minPosition.positionInt

    assertRange(positionInt, 0, fourteenBits)

    //encoding (x,y) as x*horiz+y as Int
    def x(table: Table): X = X(positionInt % table.horizontal.length)

    def y(table: Table): Y = Y(positionInt / table.horizontal.length)

    def xy(table: Table): XY = XY(x(table), y(table))

    def next(): Position = Position(positionInt + 1)
  }

  object Position {
    val zero = Position(0)

    def apply(x: X, y: Y, table: Table): Position = Position(x.x + y.y * table.horizontal.length)
  }

  final case class Table(horizontal: Horizontal, vertical: Vertical)

  final case class PositionInTable(position: Position, table: Table)

  final case class Pick(piece: Piece, position: Position) {
    override lazy val hashCode: Int = (piece, position).hashCode()
    lazy val pickInt: Int = (position.positionInt << three) + piece.pieceIndex
  }

  case class PositionSet(bitSet: BitSet) extends Iterable[Position] {
    //encoding (x,y) as x*horiz+y as Int
    def -(that: PositionSet): PositionSet = PositionSet(bitSet &~ that.bitSet)

    def +(position: Position) = PositionSet(bitSet + position.positionInt)

    def intersects(that: PositionSet): Boolean = (bitSet & that.bitSet).nonEmpty

    def filter(predicate: Int => Boolean): PositionSet = PositionSet(bitSet.filter(predicate))

    override def iterator: Iterator[Position] = bitSet.iterator.map(positionInt => Position(positionInt))
  }

  object PositionSet {
    def apply(): PositionSet = PositionSet(BitSet())

    def apply(positions: Seq[Int]): PositionSet = PositionSet(BitSet(positions: _*))
  }

  case class PartialSolution(picks: List[Pick]) {
    def +(pick: Pick): PartialSolution = PartialSolution(pick :: picks)
  }

  object PartialSolution {
    val Empty = PartialSolution(Nil)
  }

  case class BufferSize(size: Int) {
    assert(size > 0)
  }

  case class PrintEvery(size: Int) {
    assert(size > 0)
  }

  object Config {
    val bufferSize: BufferSize = BufferSize(64)
    val printEvery: PrintEvery = PrintEvery(5000000)
  }

}
