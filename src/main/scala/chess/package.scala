import chess.PositionInt

import scala.collection.{BitSet, SortedSet, immutable}

package object chess {

  case class PiecePosition(piece: Piece, position: Position)

  type PieceInt = Int //Piece ordinal
  type PositionInt = Int // high+little endian xy

  case class Input(table: Table,
                   pieces: Seq[PieceInt], //with duplicates
                   positions: SortedSet[Int])

  object Input {

    def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
      Input(table, toStream(piecesCount), positionsFor(table))

    def positionsFor(table: Table): SortedSet[Int] = {
      val positions: Seq[Int] =
        for (x <- 0 until table.horizontal;
             y <- 0 until table.vertical;
             aggNum: PositionInt = Position(x, y).toPositionInt) yield aggNum
      val emptySet: SortedSet[Int] = BitSet.empty
      positions.foldLeft(emptySet)((set: SortedSet[Int], aggNum: Int) => set + aggNum)
    }

    def toStream(piecesCount: Map[Piece, Int]): Stream[PieceInt] = {
      for (piece <- piecesCount.keys.toStream; _ <- 1 to piecesCount(piece)) yield piece.order
    }
  }

  case class Table(horizontal: Int, vertical: Int) {
    if (horizontal < 0 || horizontal >= hh || vertical < 0 || vertical >= hh)
      throw new IllegalArgumentException((horizontal,vertical) + " not within range: 0<=x<=" + hh + " and same for y")
  }

  case class PotentialSolution(solution: Set[PiecePosition])

  private val hh = 32768

  case class Position(x: Int, y: Int) {
    if (!(0 <= x && x < hh && 0 <= y && y < hh)) {
      throw new IllegalArgumentException((x, y) + " not within range: 0<=x<=" + hh + " and same for y")
    }

    def toPositionInt: PositionInt = x + y * hh
  }

  object Position {
    def fromPositionInt(xy: PositionInt): Position = Position(xy % hh, xy / hh)
  }

}
