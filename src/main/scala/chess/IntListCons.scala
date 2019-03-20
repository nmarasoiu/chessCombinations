package chess

sealed abstract class IntList {
  def toList: List[Int]
}

case object EmptyList extends IntList {
  override val toList: List[PieceCount] = Nil
}

case class IntListCons(head: Int, tail: IntList) extends IntList {
  override val toList: List[PieceCount] = head :: tail.toList
}
