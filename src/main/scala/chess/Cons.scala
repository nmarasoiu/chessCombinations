package chess

sealed abstract class PickList {
  def toList: List[Pick]
}

case object Empty extends PickList {
  override lazy val toList: List[Pick] = Nil
}

case class Cons(head: Int, tail: PickList) extends PickList {
  override lazy val toList: List[Pick] = head :: tail.toList
}
