import java.time.{Clock, Duration}

import chess.GenerationCore.solutions
import chess.Piece._
import chess._
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Properties}

object ChessProperties extends Properties("GenerationCore") {
  type Board = Set[(Piece, (Int, Int))]
  implicitly[Arbitrary[Input]]

  def rotations(table: Table, solution: Board): Set[Board] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toSet

  def rotation(table: Table, solution: Board): Board = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vertical - 1 - y, x)

    for ((piece, (x, y)) <- solution) yield (piece, rotation(x, y))
  }

  //todo: asymettric table, 0x0 tables, intro rotations in algo
  //todo: every property executes 100 times; to rewrite with ScalaTest
  property("example1") = forAll { _: Unit => {
    println("Example1:")
    val input = Input(Table(3, 3), Map[Piece, Int](King -> 2, Rook -> 1))
    val expectedBoards: Set[Board] = Set(Set((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }
  property("example2") = forAll { _: Unit => {
    println("Example2:")
    val input = Input(Table(4, 4), Map[Piece, Int](Rook -> 2, Knight -> 4))
    val expectedBoards: Set[Board] =
      Set(
        Set((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Set((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3))))

    areResultingBoardsTheExpectedOnes(input, expectedBoards)
  }
  }
  property("example3") = forAll { _: Unit => {
    println("Example3:")
    val clock = Clock.systemUTC()
    val t0 = clock.instant()
    val input = Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))
    val size = solutions(input).size
    val t1 = clock.instant()
    println(size + " computed in " + Duration.between(t0, t1))
    size > 0
  }
  }
    /*
      property("solutions") = forAll { input: Input => {
        val size = GenerationCore.solutions(input).size
        assert(size >= 0)
        size >= 0
      }
      }*/
    private def areResultingBoardsTheExpectedOnes
    (input: Input, expectedBoards: Set[Board]): Boolean
    =
    {
      val solutions = GenerationCore.solutions(input)
      val obtainedSolutions: Set[Board] = solutions.map((potentialSolution: PotentialSolution) =>
        potentialSolution.solution.map {
          case PiecePosition(piece: Piece, Position(x, y)) => (piece, (x, y))
        }).toSet
      val allExpectedBoards: Set[Board] =
        expectedBoards.flatMap(board => rotations(input.table, board))

      def evalAndStringify(boards: Iterable[Iterable[(Piece, (Int, Int))]]) = boards.mkString("\n")

      //    println("obtained " + obtainedSolutions.size)
      //    println("expectedBoards intersection with obtainedBoards=\n" + evalAndStringify(allExpectedBoards.intersect(obtainedSolutions)))
      //    println("expectedBoards - obtainedBoards=\n" + evalAndStringify(allExpectedBoards -- obtainedSolutions))
      //    println("obtainedBoards - expectedBoards=\n" + evalAndStringify(obtainedSolutions -- allExpectedBoards))
      //    println("obtained " + obtainedSolutions.size)
      obtainedSolutions.size == obtainedSolutions.size &&
        obtainedSolutions == allExpectedBoards
    }

  }