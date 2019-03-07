package chess

import chess.MonixBlockingUtil.block
import chess.Piece._

import scala.collection.immutable.Map

object MainForProfiling {
  def main(args: Array[String]): Unit = {
    val input = Input(Table(9,9), Map( King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook ->2))
    println(input)
    val solutions: Iterable[PotentialSolution] = block(GenerationCore.solutions(input))
    val obtainedSolutionCount = solutions.size
    assert(solutions.toArray.distinct.length == obtainedSolutionCount)
    assert(Set(10086030, 10206726).contains(obtainedSolutionCount)) //todo: which of these is correct? i got 10086030 with Map[Piece,Int] and with Seq[Int] 10206726
  }

}
