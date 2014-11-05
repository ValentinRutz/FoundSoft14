import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestSubst extends FunSuite with Matchers with LambdaTest {

    val s = Variable("s")
    override val reducer = reduce(_)

    test("var substitution") {
        subst(x)("x", s) should be(s)
    }

    test("var non-substitution") {
        subst(y)("x", s) should be(y)
    }

    test("no capturable vars, simple abstraction") {
        // [y->s] \x.y
        val tree = Abstraction(x, Nat, y)
        val sTree = Abstraction(x, Nat, s)
        subst(tree)("y", s) should be(sTree)
    }

    test("var shadowed in Abstraction not substituted") {
        val term = "(\\x: Nat.(\\x: Nat. x)) 0"
        val reduced = "\\x: Nat. x"
        term shouldReduceTo reduced
    }

    test("var shadowed in case inl not substituted") {
        val term = "case inl 0 as Nat+Nat of inl x => \\x: Nat.(\\x: Nat. x) | inr x => \\x: Nat.(\\x: Nat. x)"
        val reduced = "\\x: Nat.(\\x: Nat. x)"
        term shouldReduceTo reduced
    }

    test("var shadowed in case inr not substituted") {
        val term = "case inr 0 as Nat+Nat of inl x => \\x: Nat.(\\x: Nat. x) | inr x => \\x: Nat.(\\x: Nat. x)"
        val reduced = "\\x: Nat.(\\x: Nat. x)"
        term shouldReduceTo reduced
    }

}
