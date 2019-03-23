import scala.collection.immutable.BitSet

package object chess {

  final case class X(x: Int) extends AnyVal

  final case class Y(y: Int) extends AnyVal

  final case class Horizontal(length: Int) extends AnyVal

  final case class Vertical(height: Int) extends AnyVal

  final case class Count(count: Int) extends AnyVal

  /**
    * An Int wil have 32 bits:
    * - sign 1 bit
    * - piece 3 bits
    * - position: 2 * 7 bits
    * - table: 2 * 7 bits
    */
  final case class Position(pos: Int) extends AnyVal {
    //encoding (x,y) as x*horiz+y as Int
    def x(table: Table): X = X(pos % table.horizontal)

    def y(table: Table): Y = Y(pos / table.horizontal)

  }

  final case class PositionInTable(pit: Int) extends AnyVal { //2*2*7bit
    import PositionInTable._

    def position: Position = Position(pit & fourteenBits)

    def tableInt: Int = pit >>> fourteen

    def table: Table = Table(Horizontal(tableInt & sevenBits), Vertical(tableInt >>> seven))
  }

  object PositionInTable {
    val (seven, sevenBits) = (7, 128 - 1)
    val (fourteen, fourteenBits) = (14, 128 * 128 - 1)

    def apply(table: Table, position: Position): PositionInTable = PositionInTable(position.pos + (table.table << fourteen))
  }

  final case class Table(table: Int) extends AnyVal {

    def fromPairToInt(x: X, y: Y): Position = Position(x.x + y.y * horizontal)

    def horizontal: Int = table & 127

    def vertical: Int = table >> 7

  }

  object Table {
    def apply(horizontal: Horizontal, vertical: Vertical): Table = Table(horizontal.length + (vertical.height << 7))
  }

  final case class Pick(pickInt: Int) extends AnyVal { // a Piece (3bits) in a Position
    def piece(): Piece = Piece.of(pickInt & Pick.pieceEncodingOnes)

    def position(): Position = Position(pickInt >>> Pick.pieceEncodingBits)
  }

  object Pick {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def apply(piece: Piece, position: Position): Pick = Pick((position.pos << Pick.pieceEncodingBits) + piece.pieceIndex)
  }


  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = PickList // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece

  case class BufferSize(size: Int) extends AnyVal

  case class PrintEvery(size: Int) extends AnyVal

  object Config {
    val bufferSize: BufferSize = BufferSize(64)
    val printEvery: PrintEvery = PrintEvery(5000000)
  }

  implicit class RichBitSet(bitSet: Positions) {
    def intersects(other: Positions): Boolean = (bitSet & other).nonEmpty
  }

}
