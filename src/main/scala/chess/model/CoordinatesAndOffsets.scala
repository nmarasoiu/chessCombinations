package chess.model

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


abstract class LenientX extends Requiring {
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

abstract class LenientY extends Requiring {
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
