import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestReducer extends FunSuite with Matchers with LambdaTest {
    override val reducer: Term => Term = reduce

    // COMPUTATION
    test("if true") {
        "if true then true else false" shouldReduceTo "true"
    }

    test("if false") {
        "if false then true else false" shouldReduceTo "false"
    }

    test("iszero 0") {
        "iszero 0" shouldReduceTo "true"
    }

    test("iszero succ nv") {
        for (i <- 1 to 10) s"iszero $i" shouldReduceTo "false"
    }

    test("pred 0") {
        "pred 0" shouldReduceTo "0"

    }
    test("pred succ nv") {
        for (i <- 1 to 10) s"pred succ $i" shouldReduceTo s"$i"
    }

    test("(\\x: T. t) v") {
        "(\\x: Nat. x) 0" shouldReduceTo "0"
    }

    test("fst test") {
        "fst {0, tru}" shouldReduceTo "0"
    }

    test("snd test") {
        "snd {0, tru}" shouldReduceTo "tru"
    }
}
