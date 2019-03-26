package chess.model

abstract class Requiring {
  import EncodingConstants._
  def requireValueConstraints(): Unit

  def requirePositiveUpToSevenBits(minValue: Int = 0): Unit = requireRange(value, minValue, maxValue = sevenBits)

  def requireSevenBitsPlusMinusEvenBits(): Unit = requireRange(value, minValue = -sevenBits, maxValue = 2 * sevenBits)

  def value: Int

  def requireRange[T](value: Int, minValue: Int, maxValue: Int): Unit =
    require(minValue <= value && value <= maxValue,
      s"Condition $minValue <= $value <= $maxValue not respected for $getClass")
}

case class Count(value: Int) extends Requiring {
  override def requireValueConstraints(): Unit = requireRange(value, minValue = 1, maxValue = Int.MaxValue)

  def decremented(): Count = Count(value - 1)
}
