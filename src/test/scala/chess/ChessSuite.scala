package chess

import chess.BlockingUtil._
import chess.FlowableUtils._
import chess.Piece._
import org.scalatest.FunSuite

import scala.collection.immutable.Map

class ChessSuite extends FunSuite {

  case class Board(solution: Set[(Piece, (Int, Int))])

  object Table2 {
    def apply(horizontal: Int, vertical: Int): Table = Table(Horizontal(horizontal), Vertical(vertical))
  }

  test("Example 1 should return the 4 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Table2(3, 3), Map(King -> 2, Rook -> 1), Set(Set((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2)))))
  }

  test("Example 2 should return the 8 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Table2(4, 4), Map(Rook -> 2, Knight -> 4), Set(
        Set((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Set((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3)))))
  }

  test("Example 3, the variety: '1 Knight' should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table2(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 1),
      duplicationAssertion = false, executeTimes = 900, countAssertion = _ >= 3063828)
  }

  test("Example 3, the variety: '2 Knights' should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table2(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2),
      duplicationAssertion = false, executeTimes = 900, countAssertion = _ >= 2895708)
  }

  test("Example 3, the variety: '2 of each piece incl Rook' should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table2(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2),
      duplicationAssertion = false, executeTimes = 1, countAssertion = _ => true)
  }

  test("Example 7x8 should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table2(horizontal = 7, vertical = 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2),
      duplicationAssertion = false, executeTimes = 900, countAssertion = _ => true)
  }

  test("Example 8x8 should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table2(8, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2),
      duplicationAssertion = false, executeTimes = 900, countAssertion = _ => false)
  }

  def areResultingBoardsTheExpectedOnes(table: Table, pieces: Map[Piece, Int],
                                        expectedBoards: Set[Set[(Piece, (Int, Int))]]) {
    blockingTest(table, pieces, duplicationAssertion = true, executeTimes = 1, countAssertion = _ => true)
    val obtainedBoards: Iterable[List[(Piece, (Int, Int))]] =
      for (solution <- SolutionPath.solutions(table, pieces.mapValues(c => Count(c))).blockingScalaIterable())
        yield {
          for (piecePosition: Pick <- solution.picks;
               PieceAndCoordinates(piece, (x, y)) = PickTest.fromIntToPieceAndCoordinates(piecePosition, table))
            yield (piece, (x, y))
        }
    val allExpectedBoards: Set[Board] = expectedBoards.flatMap(board => rotations(table, Board(board)))

    assert(sorted(obtainedBoards) == sorted(allExpectedBoards.map(board => board.solution)))
  }

  def sorted[A](iterableOfIterable: Iterable[Iterable[A]]): IndexedSeq[String] = {
    def _sorted[B](iterable: Iterable[B]): IndexedSeq[String] = iterable.toIndexedSeq.map(_.toString).sorted

    _sorted(iterableOfIterable.map(iterable => _sorted(iterable)))
  }

  def rotations(table: Table, solution: Board): Set[Board] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toSet

  def rotation(table: Table, board: Board): Board = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vertical.height - 1 - y, x)

    Board(for ((piece, (x, y)) <- board.solution) yield (piece, rotation(x, y)))
  }

}
