import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestParser extends FunSuite with Matchers {

    def tokens(input: String) = new lexical.Scanner(input)

    implicit class testingString(input: String) {
        def =?(expectedTree: Term): Unit = {
            phrase(Term)(tokens(input)) match {
                case Success(result, _) => {
                    result should be(expectedTree)
                }
                case _ => fail
            }
        }
    }

    test("Test Parsing single var") {
        val input = "x"
        val expectedTree = Variable("x")
        input =? expectedTree
    }

    test("Test parsing simple abstraction") {
        val input = "\\x. y"
        val expectedTree = Abstraction(Variable("x"), Variable("y"))
        input =? expectedTree
    }

    test("Test parsing simple application") {
        val input = "x x"
        val expectedTree = Application(Variable("x"), Variable("x"))
        input =? expectedTree
    }

    test("Test parsing parenthesis variable") {
        val input = "(x)"
        val expectedTree = Variable("x")
        input =? expectedTree
    }
}
