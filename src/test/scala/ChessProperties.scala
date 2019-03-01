import chess.{GenerationCore, Input}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._

object ChessProperties extends Properties("GenerationCore") {
  implicitly[Arbitrary[Input]]

  //  property("startsWith") = forAll { (a: String, b: String) =>
  //    (a+b).startsWith(a)
  //  }

  property("solutions") = forAll { input: Input => {
    val size = GenerationCore.solutions(input).size
    assert(size >= 0)
    println("Generated " + size + " solutions")
    size >= 0
  }
  }
}