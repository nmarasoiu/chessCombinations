package chess

sealed abstract class PiecePositionIntList {
  def toList: List[PiecePositionInt]
}

case object EmptyList$PiecePosition extends PiecePositionIntList {
  override lazy val toList: List[PiecePositionInt] = Nil
}

case class PiecePositionIntListCons(head: Int, tail: PiecePositionIntList) extends PiecePositionIntList {
  override lazy val toList: List[PiecePositionInt] = head :: tail.toList
}
