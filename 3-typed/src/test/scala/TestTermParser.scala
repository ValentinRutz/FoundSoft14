
import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestTermParser extends FunSuite with Matchers with LambdaTest {

    test("Parse Variable") {
        "x" shouldParseTo x
    }

    test("Parse Abstraction") {
        "\\x: Nat. y" shouldParseTo Abstraction(x, Nat, y)
    }

    test("Parse Application") {
        "x x" shouldParseTo Application(x, x)
    }

    test("Parse Parenthesis") {
        "(((x)))" shouldParseTo x
    }

    test("Test parsing \"(\\x: Nat. x) y\"") {
        "(\\x: Bool. x) y" shouldParseTo Application(Abstraction(x, Bool, x), y)
    }

    test("Test parsing \"\\x: Nat.\\y: Nat. x\"") {
        "\\x: Nat.\\y: Nat. x" shouldParseTo Abstraction(x, Nat, Abstraction(y, Nat, x))
    }

    test("Test parsing \"(\\x: Nat. x) (\\y: Bool. y)\"") {
        "(\\x: Nat. x) (\\y: Bool. y)" shouldParseTo Application(Abstraction(x, Nat, x), Abstraction(y, Bool, y))
    }

    test("Test parsing \"x y z\" (left associative)") {
        "x y z" shouldParseTo Application(Application(x, y), z)
    }

    /*
    test("Test parsing \"\\x. x x x\" (abstraction max right extension)") {
        "\\x. x x x" shouldParseTo Abstraction(x, Application(Application(x, x), x))

    }

    test("Assignment example") {
        "\\x.\\y. x y x" shouldParseTo "\\x. (\\y. ((x y) x))"
    }
 */
}
