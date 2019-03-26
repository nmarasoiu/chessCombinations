package chess.model

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
