import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestParser extends FunSuite with Matchers with LambdaTest {

    test("Test Parsing \"x\"") {
        "x" shouldParseTo x
    }

    test("Test parsing \"\\x. y\"") {
        "\\x. y" shouldParseTo Abstraction(x, y)
    }

    test("Test parsing \"x x\"") {
        "x x" shouldParseTo Application(x, x)
    }

    test("Test parsing \"(((x)))\"") {
        "(((x)))" shouldParseTo x
    }

    test("Test parsing \"(\\x. x) y\"") {
        "(\\x. x) y" shouldParseTo Application(Abstraction(x, x), y)
    }

    test("Test parsing \"\\x.\\y. x\"") {
        "\\x.\\y. x" shouldParseTo Abstraction(x, Abstraction(y, x))
    }

    test("Test parsing \"(\\x. x) (\\y. y)\"") {
        "(\\x. x) (\\y. y)" shouldParseTo Application(Abstraction(x, x), Abstraction(y, y))
    }

    test("Test parsing \"x y z\" (left associative)") {
        "x y z" shouldParseTo Application(Application(x, y), z)
    }
    test("Test parsing \"\\x. x x x\" (abstraction max right extension)") {
        "\\x. x x x" shouldParseTo Abstraction(x, Application(Application(x, x), x))

    }

    test("Assignment example") {
        "\\x.\\y. x y x" shouldParseTo "\\x. (\\y. ((x y) x))"
    }

}
