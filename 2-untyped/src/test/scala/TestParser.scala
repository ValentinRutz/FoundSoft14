import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestParser extends FunSuite with Matchers {

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")

    implicit class testingString(input: String) {
        val tokens = new lexical.Scanner(input)
        def shouldBe(expectedTree: Term): Unit = {
            phrase(Term)(tokens) match {
                case Success(result, _) => {
                    result should be(expectedTree)
                }
                case Failure(msg, _) => fail(msg)
                case _ => fail("something went really wrong")
            }
        }
    }

    test("Test Parsing \"x\"") {
        "x" shouldBe x
    }

    test("Test parsing \"\\x. y\"") {
        "\\x. y" shouldBe Abstraction(x, y)
    }

    test("Test parsing \"x x\"") {
        "x x" shouldBe Application(x, x)
    }

    test("Test parsing \"(((x)))\"") {
        "(((x)))" shouldBe x
    }

    test("Test parsing \"(\\x. x) y\"") {
        "(\\x. x) y" shouldBe Application(Abstraction(x, x), y)
    }

    test("Test parsing \"\\x.\\y. x\"") {
        "\\x.\\y. x" shouldBe Abstraction(x, Abstraction(y, x))
    }

    test("Test parsing \"(\\x. x) (\\y. y)\"") {
        "(\\x. x) (\\y. y)" shouldBe Application(Abstraction(x, x), Abstraction(y, y))
    }

    test("Test parsing \"x y z\" (left associative)") {
        "x y z" shouldBe Application(Application(x, y), z)
    }
    test("Test parsing \"\\x. x x x\" (abstraction max right extension)") {
        "\\x. x x x" shouldBe Abstraction(x, Application(Application(x, x), x))

    }

}
