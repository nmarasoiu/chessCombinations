package chess.model

case class Table(horizontal: Horizontal, vertical: Vertical) {
  def area: Int = vertical.height * horizontal.length
}

case class Horizontal(length: Int) extends Requiring {
  override def value: Int = length

  override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits(minValue = 1)
}

case class Vertical(height: Int) extends Requiring {
  override def value: Int = height

  override def requireValueConstraints(): Unit = requirePositiveUpToSevenBits(minValue = 1)
}