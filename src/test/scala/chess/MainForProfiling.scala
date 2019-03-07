package chess

import chess.MonixBlockingUtil.block
import chess.Piece._

import scala.collection.immutable.Map

object MainForProfiling {
  def main(args: Array[String]): Unit = {
    val input = Input(Table(9,9), Map( King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook ->2))
    println(input)
    block(GenerationCore.solutions(input))
  }

}
