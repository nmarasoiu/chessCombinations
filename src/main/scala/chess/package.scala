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

  def assertRange(value:Int,minValue:Int, maxValue:Int): Boolean = minValue <= value && value <= maxValue
  
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

  final case class Position(pos: Int) {
    assertRange(pos, 0, fourteenBits)

    //encoding (x,y) as x*horiz+y as Int
    def x(table: Table): X = X(pos % table.horizontal)

    def y(table: Table): Y = Y(pos / table.horizontal)

  }


  final case class PositionInTable(pit: Int) { //2*2*7bit
    assertRange(pit, 0, 268435455)

    def position: Position = Position(pit & fourteenBits)

    def tableInt: Int = pit >>> fourteen

    def table: Table = Table(Horizontal(tableInt & sevenBits), Vertical(tableInt >>> seven))
  }

  object PositionInTable {

    def apply(table: Table, position: Position): PositionInTable = PositionInTable(position.pos + (table.table << fourteen))
  }

  final case class Table(table: Int) {
    assertRange(table, 1, fourteenBits)

    def fromPairToInt(x: X, y: Y): Position = Position(x.x + y.y * horizontal)

    def horizontal: Int = table & 127

    def vertical: Int = table >> 7
  }

  object Table {
    def apply(horizontal: Horizontal, vertical: Vertical): Table = Table(horizontal.length + (vertical.height << 7))
  }

  final case class Pick(pickInt: Int) { // a Piece (3bits) in a Position
    assertRange(pickInt, 0, 2147483647)

    def piece(): Piece = Piece.of(pickInt & threeBits)

    def position(): Position = Position(pickInt >>> three)
  }

  object Pick {
    def apply(piece: Piece, position: Position): Pick = Pick((position.pos << three) + piece.pieceIndex)
  }

  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = PickList // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece

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
