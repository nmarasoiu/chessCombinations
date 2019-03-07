package chess

import chess.MonixBlockingUtil.block
import chess.Piece._

import scala.collection.immutable.Map

object MainForProfiling {
  def main(args: Array[String]): Unit = {
    block(GenerationCore.solutions(Input(Table(7,7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2))), checkDuplication = false)
    block(GenerationCore.solutions(Input(Table(8, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))), checkDuplication = false)
    block(GenerationCore.solutions(Input(Table(9, 8), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))), checkDuplication = false)
    block(GenerationCore.solutions(Input(Table(9, 9), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2, Rook -> 2))), checkDuplication = false)
  }

}
