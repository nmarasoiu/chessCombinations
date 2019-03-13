package chess

import chess.BlockingUtil.executeAndBlock
import chess.Piece._
import org.scalatest.FunSuite

import scala.collection.immutable.{Map, SortedSet, TreeSet}

class ChessSuite extends FunSuite {

  type Board = Set[(Piece, (Int, Int))]

  test("Example 1 should return the 4 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Input(Table(3, 3), Map(King -> 2, Rook -> 1)),
      Set(Set((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2)))))
  }
  test("Example 2 should return the 8 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Input(Table(4, 4), Map(Rook -> 2, Knight -> 4)),
      Set(
        Set((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Set((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3)))))
  }

  test("Example 3 should return the solutions, and that there are no duplicates in the returned solutions") {
    executeAndBlock(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2)))
  }

  test("Example 4 should return the solutions, and that there are no duplicates in the returned solutions") {
    executeAndBlock(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2)))
  }

  test("Example 5 should return the solutions, and that there are no duplicates in the returned solutions") {
    executeAndBlock(Input(Table(7, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2)))
  }

  test("Example 6 should return the solutions, and that there are no duplicates in the returned solutions") {
    executeAndBlock(Input(Table(8, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2)))
  }

  def areResultingBoardsTheExpectedOnes(input: Input, expectedBoards: Set[Board]) {
    val obtainedBoards: Iterable[Board] =
      for (solution <- executeAndBlock(input, checkDuplication = true))
        yield {
          for (piecePosition <- solution;
               PieceAndCoordinates(piece, (x, y)) = PiecePosition.fromIntToPieceAndCoordinates(piecePosition, input.table))
            yield (piece, (x, y))
        }
    val allExpectedBoards: Set[Board] = expectedBoards.flatMap(board => rotations(input.table, board))

    assert(obtainedBoards.size == allExpectedBoards.size)
    val obtainedSet = obtainedBoards.toSet

    def sorted[T](obtainedSet: Set[Set[T]])(implicit o: Ordering[T]): SortedSet[SortedSet[T]] = {
      def _sorted[U](set: Set[U])(implicit o: Ordering[U]): SortedSet[U] = {
        TreeSet[U]() ++ set
      }

      implicit val setOrdering: Ordering[SortedSet[T]] = (x, y) => x.toString.compare(y.toString)
      _sorted(obtainedSet.map(s => _sorted(s)))
    }

    assert(sorted(obtainedSet) == sorted(allExpectedBoards))
  }

  def rotations(table: Table, solution: Board): Set[Board] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toSet

  def rotation(table: Table, solution: Board): Board = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vertical - 1 - y, x)

    for ((piece, (x, y)) <- solution) yield (piece, rotation(x, y))
  }

}
