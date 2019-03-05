package chess

import java.time.{Clock, Duration}

import chess.Piece.{Bishop, King, Knight, Queen}

object GenerationCore {
  /**
    * todo:
    * in parallel course grained, split the table
    * scala test, add tests
    * profile with their bigger example
    * anything to cache/reuse, dynamic prog?
    * refactor into a single for and get rid of flatten?
    * inspect & remove todos
    * refactoring, beautiful well organized code
    * memory pressure, balance lazy with eager
    * cearsaf de pus la plapuma; de spalat coco in fiecare seara
    * too many sets? preallocate?
    */
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Seq[PotentialSolution] = {
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  //todo start not with set but with seq
  private def _solutions(input: Input, picksSoFar: Set[Position]): Seq[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: Set[Position]) = input
    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Seq()
    } else {
      val piece = pieces.head
      val remainingPieces = pieces.tail
      //todo refactor into a single for and get rid of flatten?
      val r: Seq[Seq[PotentialSolution]] =
        for (position <- positions.toSeq;
             incompatiblePositions = piece.incompatiblePositions(position, table);
             _ <- Set(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
             remainingPositions = positions - position -- incompatiblePositions)
          yield
            if (remainingPieces.isEmpty) {
              val potentialSolution = PotentialSolution(Set((piece, position)))
              Seq(potentialSolution)
            } else {
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar + position)

              remainingPotentialSolutions.map(remainingPotentialSolution => {
                val potentialSolution = PotentialSolution(Set((piece, position)) ++ remainingPotentialSolution.solution)
                potentialSolution
              })
            }
      r.flatten
    }
  }

  def main(args: Array[String]): Unit = {
    val clock = Clock.systemUTC()
    val t0 = clock.instant()
    val input = Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))
    val size = solutions(input).size
    val t1 = clock.instant()
    println(size + " computed in " + Duration.between(t0, t1))
  }
}

case class Input(table: Table,
                 pieces: Seq[Piece], //with duplicates
                 positions: Set[Position])

object Input {
  def apply(table: Table, piecesCount: Map[Piece, Int]): Input =
    apply(table, toSeq(piecesCount), positionsFor(table).toSet)

  def positionsFor(table: Table): Seq[Position] = {
    for (i <- 0 until table.horizontal; j <- 0 until table.vertical) yield Position(i, j)
  }

  def toSeq(piecesCount: Map[Piece, Int]): Seq[Piece] = {
    for (piece <- piecesCount.keys.toSeq; _ <- 1 to piecesCount(piece)) yield piece
  }
}

case class Position(x: Int, y: Int)

case class Table(horizontal: Int, vertical: Int)

case class PotentialSolution(solution: Set[(Piece, Position)])
