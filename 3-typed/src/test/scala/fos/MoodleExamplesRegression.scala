import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class MoodleExamplesRegression extends FunSuite with Matchers {

    def parse(in: String): Term = {
        val tokens = new lexical.Scanner(in)
        phrase(Term)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    test("Type checking when there is no type") {
        an[TypeError] should be thrownBy typeof(Nil, parse("x y"))
    }

    test("order of rules in call-by-value") {
        val input = """(\x:Bool.(\y:Bool.y) true) 0"""
        val topTerm = parse(input)

        topTerm.toString should be(input)
        reduce(topTerm).toString should be("""(\y:Bool.y) true""")
    }
}
