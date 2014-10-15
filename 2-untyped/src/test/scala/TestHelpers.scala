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

    val s1: Variable = Variable("s$1")

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

    test("abstraction substitution") {
        // [x->s] x y
        resetFreshName
        val tree = Application(x, y)
        val sTree = Application(s, y)
        subst(tree)("x", s) should be(sTree)
    }

    test("captured unused var") {
        // [x->s] \s.s
        resetFreshName
        val tree = Abstraction(s, s)
        val sTree = Abstraction(s1, s1)
        subst(tree)("x", s) should be(sTree)
    }

    test("generated fresh name is not fresh") {
        // [x->s] \s$1.\s.x
        pending
        val s2 = Variable("s$2")
        resetFreshName
        val tree = Abstraction(s1, Abstraction(s, x))
        val sTree = Abstraction(s1, Abstraction(s2, x))
        subst(tree)("x", s) should be(sTree)
    }
}
