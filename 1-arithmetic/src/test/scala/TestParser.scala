import fos.Arithmetic._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestParser extends FunSuite with Matchers {

    test("Example 1 from lab statement") {
        val input = "if iszero pred pred 2 then if iszero 0 then true else false else false"
        val output = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)

        parse(input) should be(Some(output))
    }

    test("Example with succ") {
        val input = "if iszero pred pred succ 1 then if iszero 0 then true else false else false"
        val output = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)

        parse(input) should be(Some(output))
    }

    test("Example 2 from lab statement") {
        val input = "pred succ succ succ false"
        val output = Pred(Succ(Succ(Succ(False))))

        parse(input) should be(Some(output))
    }

    test("Simple parsing") {
        val simpleInputs = Map(
            "true" -> True,
            "false" -> False,
            "0" -> Zero,
            "1" -> Succ(Zero),
            "if true then true else true" -> If(True, True, True),
            "succ 0" -> Succ(Zero),
            "pred 0" -> Pred(Zero),
            "iszero 0" -> IsZero(Zero))

        for ((input, output) <- simpleInputs) {
            parse(input) should be(Some(output))
        }

    }

    test("Negative testing") {
        val badInputs = Seq("true true", "succ succ", "false succ",
            "numericLit", "if if then true else 0", "00l")

        for (input <- badInputs) { parse(input) should be(None) }
    }

    def parse(str: String): Option[Term] = {
        val tokens = new lexical.Scanner(str)
        phrase(Expr)(tokens) match {
            case Success(trees, _) => Some(trees)
            case _ => None
        }
    }
}
