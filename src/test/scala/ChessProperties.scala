import chess.ChessPiece._
import chess._
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Properties}

object ChessProperties extends Properties("GenerationCore") {
  implicitly[Arbitrary[Input]]

  def rotations(table: Table, solution: Seq[(ChessPiece, (Int, Int))]): Seq[Seq[(ChessPiece, (Int, Int))]] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4)

  def rotation(table: Table, solution: Seq[(ChessPiece, (Int, Int))]): Seq[(ChessPiece, (Int, Int))] = {
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
    val expectedBoards: Seq[Seq[(ChessPiece, (Int, Int))]] = Stream(Stream((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }

  property("example2") = forAll { _: Unit => {
    println("Example2:")
    val input = Input(Table(4, 4), Map(Rook -> 2, Knight -> 4))
    val expectedBoards: Seq[Seq[(ChessPiece, (Int, Int))]] =
      Stream(
        Stream((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Stream((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }

  private def areResultingBoardsTheExpectedOnes
  (input: Input, expectedBoards: Seq[Seq[(ChessPiece, (Int, Int))]]): Boolean = {
    val solutions = GenerationCore.solutions(input)
    val obtainedSolutions = solutions.map(_.solution.map {
      case (piece, Position(x, y)) => (piece, (x, y))
    })
    val allExpectedBoards = expectedBoards.flatMap(board => rotations(input.table, board))
    def evalAndStringify(boards: Seq[Seq[(ChessPiece, (Int, Int))]]) =
      boards.toList.map(_.toList).mkString("\n")
    println("expectedBoards=\n" + evalAndStringify(allExpectedBoards))
    println("obtainedBoards=\n" + evalAndStringify(obtainedSolutions))
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