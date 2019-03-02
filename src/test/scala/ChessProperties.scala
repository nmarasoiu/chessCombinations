import chess.ChessPiece._
import chess._
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Properties}

object ChessProperties extends Properties("GenerationCore") {
  implicitly[Arbitrary[Input]]

  def rotations(table: Table, solution: List[(ChessPiece, (Int, Int))]): List[List[(ChessPiece, (Int, Int))]] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toList

  def rotation(table: Table, solution: List[(ChessPiece, (Int, Int))]): List[(ChessPiece, (Int, Int))] = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vert - 1 - y, x)

    for ((piece, (x, y)) <- solution) yield (piece, rotation(x, y))
  }

  //todo: asymettric table, 0x0 tables, intro rotations in algo
  /**
    * 00 10
    * 01 00
    * 11 01
    * 10 11
    *
    */
  property("example1") = forAll { _: Unit => {
    println("Example1:")
    val input = Input(Table(3, 3), Map(King -> 2, Rook -> 1))
    val expectedBoards: List[List[(ChessPiece, (Int, Int))]] = List(List((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }

  property("example2") = forAll { _: Unit => {
    println("Example2:")
    val input = Input(Table(4, 4), Map(Rook -> 2, Knight -> 4))
    val expectedBoards: List[List[(ChessPiece, (Int, Int))]] =
      List(
        List((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        List((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }

  private def areResultingBoardsTheExpectedOnes
  (input: Input, expectedBoards: List[List[(ChessPiece, (Int, Int))]]): Boolean = {
    val solutions = GenerationCore.solutions(input)
    val obtainedSolutions = solutions.map(_.solution.toList.map {
      case (piece, Position(x, y)) => (piece, (x, y))
    })
    val allExpectedBoards = expectedBoards.flatMap(board => rotations(input.table, board))
    println("expectedBoards=\n" + allExpectedBoards.mkString("\n"))
    println("obtainedBoards=\n" + obtainedSolutions.mkString("\n"))
    obtainedSolutions.toSet == allExpectedBoards.toSet
  }


  /*
    property("solutions") = forAll { input: Input => {
      val size = GenerationCore.solutions(input).size
      assert(size >= 0)
      println("Generated " + size + " solutions")
      size >= 0
    }
    }
  */
}