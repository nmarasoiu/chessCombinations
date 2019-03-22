import scala.collection.immutable.{BitSet, Map}

package object chess {

  //todo check that all Int/Long/primitive appearances that can be replaced with typed values are replaced so:) for expressiveness, clarity, type safety, performance & memory
  //todo check to convert and classes / case classes with a single primitive member to AnyVal
  //todo convert Table, PIT, etc to AnyVal :)
  final case class CoordinateX(x: Int) extends AnyVal

  final case class CoordinateY(y: Int) extends AnyVal

  final case class PieceId(pieceInt: Int) extends AnyVal {
    def piece(): Piece =Piece.of(this)
  }

  final case class PieceCount(count: Int) extends AnyVal {
  }

  final case class Position(pos: Int) extends AnyVal //encoding (x,y) as x*horiz+y as Int
  /**
    * An Int wil have 32 bits:
    * - sign 1 bit
    * - piece 3 bits
    * - position: 2 * 7 bits
    * - table: 2 * 7 bits
    */
  final case class PositionInTable(pit: Int) extends AnyVal { //2*2*7bit
    import PositionInTable._

    def position: Position = Position(pit & fourteenBits)

    def tableInt: Int = pit >>> fourteen

    def table: Table = Table(tableInt & sevenBits, tableInt >>> seven)
  }

  object PositionInTable {
    val (seven, sevenBits) = (7, 128 - 1)
    val (fourteen, fourteenBits) = (14, 128 * 128 - 1)

    def apply(table: Table, position: Position): PositionInTable = PositionInTable(position.pos + (table.table << fourteen))
  }
  final case class Table(table: Int) extends AnyVal {
//    def fromPairToInt(x: Int, y: Int): Position = Position(x + y * horizontal)

    def fromPairToInt(x: CoordinateX, y: CoordinateY): Position = Position(x.x + y.y * horizontal)

    def horizontal: Int = table & 127
    def vertical: Int = table >> 7

    def fromIntToPair(position: Position): (CoordinateX, CoordinateY) =
      (CoordinateX(position.pos % horizontal), CoordinateY(position.pos / horizontal))
  }

  object Table {
    def apply(horizontal: Int, vertical: Int): Table = Table(horizontal + (vertical << 7))
  }

  final case class Pick(pickInt: Int) extends AnyVal { // a Piece (3bits) in a Position
    //    def piece: Piece = PiecePosition.fromIntToPieceAndCoordinates(pick)
    //
    //    def positionInTable: PositionInTable =
  }

  object Pick {
    private val pieceEncodingBits = 3
    private val pieceEncodingOnes = (1 << pieceEncodingBits) - 1

    def piece(piecePosition: Pick): Piece = Piece.of(PieceId(piecePosition.pickInt & pieceEncodingOnes))

    def position(piecePositionInt: Pick): Position = Position(piecePositionInt.pickInt >>> pieceEncodingBits)

    def toInt(piece: Piece, position: Position): Pick = Pick((position.pos << Pick.pieceEncodingBits) + piece.order.pieceInt)
  }


  type Positions = BitSet //encoding (x,y) as x*horiz+y as Int
  type Solution = PickList // encoding Piece at (x,y) as x*horiz+y as Int followed by 3 bits piece

  object Config {
    val bufferSize: Int = 64
    val printEvery: Int = 5000000
  }

  implicit class RichBitSet(bitSet: Positions) {
    def intersects(other: Positions): Boolean = (bitSet & other).nonEmpty
  }

  implicit class RichMap[K, V](map: Map[K, V])(implicit cmp: Ordering[K]) {
    def minOption(): Option[(K, V)] = {
      val none: Option[(K, V)] = None
      map.foldLeft(none) {
        case (None, kv) => Some(kv)
        case (Some((k1, v1)), (k2, v2)) => if (cmp.compare(k1, k2) <= 0) Some(k1, v1) else Some(k2, v2)
      }
    }
  }

}
