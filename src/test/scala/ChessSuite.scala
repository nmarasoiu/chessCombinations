import java.time.Clock

import MonixBlockingUtil.block
import chess.Piece._
import chess._
import org.scalatest.FunSuite

class ChessSuite extends FunSuite {

  type Board = Set[(Piece, (Int, Int))]

  test("Example 1 should return the 4 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Input(Table(3, 3), Map[Piece, Int](King -> 2, Rook -> 1)),
      Set(Set((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2)))))
  }
  test("Example 2 should return the 8 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Input(Table(4, 4), Map[Piece, Int](Rook -> 2, Knight -> 4)),
      Set(
        Set((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Set((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3)))))
  }
  test("Example 3 should return the 10.2M solutions with no duplicates") {
    val clock = Clock.systemUTC()
    val t0 = clock.instant()
    val input = Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))
    val solutions: Iterable[PotentialSolution] = block(GenerationCore.solutions(input))
    val obtainedSolutionCount = solutions.size
    val t1 = clock.instant()
    println(" computed in " + java.time.Duration.between(t0, t1) + " -> " + obtainedSolutionCount + " solutions found")
    assert(solutions.toArray.distinct.length == obtainedSolutionCount)
    assert(obtainedSolutionCount == 10206726)
  }

  def areResultingBoardsTheExpectedOnes(input: Input, expectedBoards: Set[Board]) {
    val obtainedBoards: Iterable[Board] = block(GenerationCore.solutions(input))
      .map((potentialSolution: PotentialSolution) =>
        potentialSolution.solution.map {
          case PiecePosition(piece: Piece, xy) => (piece, fromPosition(xy))
        })
    val allExpectedBoards: Set[Board] = expectedBoards.flatMap(board => rotations(input.table, board))

    //    def evalAndStringify(boards: Iterable[Iterable[(Piece, (Int, Int))]]) = boards.mkString("\n")
    //    println("expectedBoards intersection with obtainedBoards=\n" + evalAndStringify(allExpectedBoards.intersect(obtainedBoards)))
    //    println("expectedBoards - obtainedBoards=\n" + evalAndStringify(allExpectedBoards -- obtainedBoards))
    //    println("obtainedBoards - expectedBoards=\n" + evalAndStringify(obtainedBoards -- allExpectedBoards))
    assert(obtainedBoards.size == allExpectedBoards.size)
    val obtainedSet = obtainedBoards.toSet
    assert(Set.empty == obtainedSet -- allExpectedBoards)
    assert(Set.empty == allExpectedBoards -- obtainedSet)
    assert(obtainedSet == allExpectedBoards)
  }

  def rotations(table: Table, solution: Board): Set[Board] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toSet

  def rotation(table: Table, solution: Board): Board = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vertical - 1 - y, x)

    for ((piece, (x, y)) <- solution) yield (piece, rotation(x, y))
  }

}