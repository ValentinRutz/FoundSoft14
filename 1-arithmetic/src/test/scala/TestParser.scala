import fos.Arithmetic._
import fos._
import org.scalatest.FunSuite

class TestParser extends FunSuite {

    test("First example from statement") {
        val input = "if iszero pred pred 2 then if iszero 0 then true else false else false"
        val output = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)
        assert(launchTest(input, output))
    }

    test("Example with succ") {
        val input = "if iszero pred pred succ 1 then if iszero 0 then true else false else false"
        val output = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)
        assert(launchTest(input, output))
    }

    def launchTest(str: String, ast: Term): Boolean = {
        val tokens = new lexical.Scanner(str)
        phrase(Expr)(tokens) match {
            case Success(trees, _) => trees == ast
            case _ => false
        }
    }
}