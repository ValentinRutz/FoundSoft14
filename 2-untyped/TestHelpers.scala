import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestFreeVars extends FunSuite with Matchers {
    val x: Variable = Variable("x")
    val y: Variable = Variable("y")
    test("var test") {
        freeVars(x) should be(Set(x))
    }

    test("application test") {
        freeVars(Application(x, y)) should be(Set(x, y))
    }

    test("abstraction test") {
        freeVars(Abstraction(x, Application(x, y))) should be(Set(y))
    }
}
