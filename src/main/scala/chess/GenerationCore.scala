package chess

import java.time.{Clock, Duration}

import chess.Piece.{Bishop, King, Knight, Queen}

import scala.collection.{SortedSet, immutable}

object GenerationCore {
  /**
    * todo:
    * remove streams: replace Stream[Piece] with Map[Piece,Int] and Stream[PotentialSolution] and any others
    * in parallel course grained, split the table
    * scala test, add tests
    * anything to cache/reuse, dynamic prog?
    * refactor into a single for and get rid of flatten?
    * inspect & remove todos
    * refactoring, beautiful well organized code
    * memory pressure, balance lazy with eager
    * too many sets? preallocate?
    */
  //todo in parallel? thread safe? .par but..
  def solutions(input: Input): Stream[PotentialSolution] = {
    _solutions(input, Set()).filter(sol => sol.solution.size == input.pieces.size)
  }

  private def _solutions(input: Input, picksSoFar: Set[Position]): Stream[PotentialSolution] = {
    val Input(table, pieces: Seq[Piece], positions: SortedSet[PositionInt]) = input
    if (pieces.isEmpty || table.vertical <= 0 || table.horizontal <= 0) {
      Stream()
    } else {
      val piece: Piece = pieces.head
      val remainingPieces: Seq[Piece] = pieces.tail
      val r: Stream[Stream[PotentialSolution]] =
        for (positionInt: PositionInt <- positions.toStream;
             position = Position.fromPositionInt(positionInt);
             incompatiblePositions = piece.incompatiblePositions(position, table).map(_.toPositionInt);
             _ <- Seq(1) if !picksSoFar.exists(otherPosition => piece.takes(position, otherPosition));
             remainingPositions = positions - positionInt -- incompatiblePositions)
          yield {
            val piecePosition = PiecePosition(piece, position)
            if (remainingPieces.isEmpty) {
              val potentialSolution = PotentialSolution(Set(piecePosition))
              Stream(potentialSolution)
            } else {
              val remainingInput = Input(table, remainingPieces, remainingPositions)
              val remainingPotentialSolutions = _solutions(remainingInput, picksSoFar + position)

              remainingPotentialSolutions.map(remainingPotentialSolution => {
                PotentialSolution(remainingPotentialSolution.solution + piecePosition)
              })
            }
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
