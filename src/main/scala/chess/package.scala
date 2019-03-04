package object chess {
  case class Position(x:Int,y:Int)

  case class Table(horiz: Int, vert: Int)

  case class Solution(solution: Stream[(ChessPiece, Position)])


}
