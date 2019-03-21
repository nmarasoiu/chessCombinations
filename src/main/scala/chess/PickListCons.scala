package chess

sealed abstract class PickList {
  def toList: List[Pick]
}

case object EmptyList$PiecePosition extends PickList {
  override lazy val toList: List[Pick] = Nil
}

case class PickListCons(head: Int, tail: PickList) extends PickList {
  override lazy val toList: List[Pick] = head :: tail.toList
}
