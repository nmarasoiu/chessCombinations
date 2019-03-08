package chess

import chess.MonixBlockingUtil.block
import chess.Piece._

import scala.collection.immutable.Map

object MainForProfiling {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to Int.MaxValue) {
      block(GenerationCore.solutions(Input(Table(3, 3), Map(King -> 2, Rook -> 1))))
      block(GenerationCore.solutions(Input(Table(4, 4), Map(Rook -> 2, Knight -> 4))))
      block(GenerationCore.solutions(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))))//50s //todo add to tests too
      block(GenerationCore.solutions(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))), checkDuplication = false)//15s
    }
  }
}