package chess

import chess.BlockingUtil._
import chess.Piece._
import org.scalatest.FunSuite

import scala.collection.immutable.Map

class ChessSuite extends FunSuite {

  type Board = Set[(Piece, (Int, Int))]

  test("Example 1 should return the 4 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Table(3, 3), Map(King -> 2, Rook -> 1),
      Set(Set((Rook, (1, 0)), (King, (0, 2)), (King, (2, 2)))))
  }
  test("Example 2 should return the 8 solutions and no other solution and no duplicate solution") {
    areResultingBoardsTheExpectedOnes(
      Table(4, 4), Map(Rook -> 2, Knight -> 4),
      Set(
        Set((Rook, (0, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (2, 2)), (Knight, (1, 3)), (Knight, (3, 3))),
        Set((Rook, (2, 0)), (Knight, (1, 1)), (Knight, (3, 1)), (Rook, (0, 2)), (Knight, (1, 3)), (Knight, (3, 3)))))
  }

  test("Example '1 Knight' should return the solutions, and that there are no duplicates in the returned solutions") {
    while (true)
      assert(blockingTest(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 1)) >= 3063828)
  }

  test("Example '2 Knights' should return the solutions, and that there are no duplicates in the returned solutions") {
    assert(blockingTest(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2)) >= 2895708)
  }

  test("Example '2 of each piece' should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))
  }

  test("Example 7x8 should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table(7, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))
  }

  test("Example 8x8 should return the solutions, and that there are no duplicates in the returned solutions") {
    blockingTest(Table(8, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))
  }

  def areResultingBoardsTheExpectedOnes(table: Table, piecesToPositions: Map[Piece, Position], expectedBoards: Set[Board]) {
    blockingTest(table, piecesToPositions, checkDuplication = true)
    val input = Input.from(table, piecesToPositions)
    val obtainedBoards: Iterable[Board] =
      for (solution <- blockingIterable(input))
        yield {
          import scala.collection.JavaConverters._
          for (piecePosition: Int <- solution.toSet;
               PieceAndCoordinates(piece, (x, y)) = PiecePosition.fromIntToPieceAndCoordinates(piecePosition, input.table))
            yield (piece, (x, y))
        }
    val allExpectedBoards: Set[Board] = expectedBoards.flatMap(board => rotations(input.table, board))

    assert(obtainedBoards.size == allExpectedBoards.size)
    val obtainedSet = obtainedBoards.toSet

    assert(Utils.sorted(obtainedSet) == Utils.sorted(allExpectedBoards))
  }

  def rotations(table: Table, solution: Board): Set[Board] =
    Stream.iterate(solution)(solution => rotation(table, solution)).take(4).toSet

  def rotation(table: Table, solution: Board): Board = {
    def rotation(x: Int, y: Int): (Int, Int) = (table.vertical - 1 - y, x)

    for ((piece, (x, y)) <- solution) yield (piece, rotation(x, y))
  }

}
