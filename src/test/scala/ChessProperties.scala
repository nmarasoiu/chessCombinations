import chess.ChessPiece._
import chess.{GenerationCore, Input, Table}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._

object ChessProperties extends Properties("GenerationCore") {
  implicitly[Arbitrary[Input]]
/*
  property("solutions") = forAll { input: Input => {
    val size = GenerationCore.solutions(input).size
    assert(size >= 0)
    println("Generated " + size + " solutions")
    size >= 0
  }
  }
*/

  property("example1") = forAll { _: Unit => {
    println("Example1:")
    val solutions = GenerationCore.solutions(Input(Table(3, 3), Map(King -> 2, Rook -> 1)))
    println("Example1: solutions: \n"+solutions.mkString("\n"))
    solutions.size == 4
  }
  }
  property("example2") = forAll { _: Unit => {
    println("Example2:")
    val solutions = GenerationCore.solutions(Input(Table(4, 4), Map(Rook -> 2, Knight -> 4)))
    println("Example2: solutions: \n"+solutions.mkString("\n"))
    solutions.size == 8
  }
  }
}