package chess

import chess.MonixBlockingUtil.block
import chess.Piece._

import scala.collection.immutable.Map

object MainForProfiling {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to Int.MaxValue) {
      for (_ <- 1 to 2) {
        for (_ <- 1 to 2) {
          for (_ <- 1 to 2) {
            for (_ <- 1 to 2) {
              block(GenerationCore.solutions(Input(Table(3, 3), Map(King -> 2, Rook -> 1))))
            }
            block(GenerationCore.solutions(Input(Table(4, 4), Map(Rook -> 2, Knight -> 4))))
          }
          block(GenerationCore.solutions(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))))
        }
      }
      block(GenerationCore.solutions(Input(Table(7, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))))
    }
  }
}