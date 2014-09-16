import fos.Arithmetic._
import fos._
import org.scalatest.FunSuite

class TestParser extends FunSuite {
 
  test("First example from statement") {
    val input = "if iszero pred pred 2 then if iszero 0 then true else false else false"
    val output = If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
    assert(test(input, output))
  }

  def test(str: String, ast: Term): Boolean = {
    val tokens = new lexical.Scanner(str)
    phrase(Expr)(tokens) match {
      case Success(trees, _) => trees == ast
      case _ => false
    }
  }
}