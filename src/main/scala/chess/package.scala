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

  final case class X(x: Int) {
    assertRange(x, minValue = 0, maxValue = sevenBits)
  }

  final case class Y(y: Int) {
    assertRange(y, minValue = 0, maxValue = sevenBits)
  }

  final case class Horizontal(length: Int) {
    assertRange(length, minValue = 1, maxValue = sevenBits)
  }

  final case class Vertical(height: Int) {
    assertRange(height, minValue = 1, maxValue = sevenBits)
  }

  final case class Count(count: Int) {
    assertRange(count, minValue = 1, maxValue = sevenBits)
  }

  final case class Position(positionInt: Int) {
    assertRange(positionInt, 0, fourteenBits)

    //encoding (x,y) as x*horiz+y as Int
    def x(table: Table): X = X(positionInt % table.horizontal.length)

    def y(table: Table): Y = Y(positionInt / table.horizontal.length)
  }

  object Position {
    def apply(x: X, y: Y, table: Table): Position = Position(x.x + y.y * table.horizontal.length)
  }

  final case class Table(horizontal: Horizontal, vertical: Vertical)

  final case class PositionInTable(position: Position, table: Table)

  final case class Pick(piece:Piece, position: Position) {
    lazy val pickInt: Int = (position.positionInt << three) + piece.pieceIndex
  }

  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = List[Pick] // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece

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

  implicit class RichBitSet(bitSet: Positions) {
    def intersects(other: Positions): Boolean = (bitSet & other).nonEmpty
  }

}
