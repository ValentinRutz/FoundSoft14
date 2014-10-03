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

class TestSubstAlpha extends FunSuite with Matchers {
    val s: Variable = Variable("s")
    val x: Variable = Variable("x")
    val y: Variable = Variable("y")
    val z: Variable = Variable("z")

    val s1: Variable = Variable("s1")

    def resetFreshName: Unit = freshName.counter = 0

    test("var substitution") {
        subst(x)("x", s) should be(s)
    }

    test("var non-substitution") {
        subst(y)("x", s) should be(y)
    }

    test("no capturable vars, simple abstraction") {
        // [y->s] \x.y
        val tree = Abstraction(x, y)
        val sTree = Abstraction(x, s)
        subst(tree)("y", s) should be(sTree)
    }

    test("captured var in simple abstraction") {
        // [x->s] \s.x
        resetFreshName
        val tree = Abstraction(s, x)
        val sTree = Abstraction(s1, s)
        subst(tree)("x", s) should be(sTree)
    }
}
