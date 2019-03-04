package object chess {

  case class Position(x: Int, y: Int)

  case class Table(horiz: Int, vert: Int)

  case class PotentialSolution(solution: List[(Piece, Position)]) {
//    def isAnActualSolution(numberOfPiecesIncludingDuplicates: Int): Boolean = {
//      solution.size == numberOfPiecesIncludingDuplicates
//    }
  }

}
