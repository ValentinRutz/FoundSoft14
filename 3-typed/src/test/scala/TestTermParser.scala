
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

    test("Test parsing \"\\x: Nat. x x x\" (abstraction max right extension)") {
        "\\x: Nat. x x x" shouldParseTo Abstraction(x, Nat, Application(Application(x, x), x))

    }

    test("Assignment example") {
        "\\x: Nat.\\y: Nat. x y x" shouldParseTo "\\x: Nat. (\\y: Nat. ((x y) x))"
    }

    test("Parse 0") {
        "0" shouldParseTo Zero()
    }

    test("Parse succ") {
        "succ 0" shouldParseTo Succ(Zero())
    }

    test("Parse pred") {
        "pred 0" shouldParseTo Pred(Zero())
    }

    test("Parse pred succ 1") {
        "pred succ 1" shouldParseTo Pred(Succ(Succ(Zero())))
    }

    test("Parse Int") {
        "1" shouldParseTo Succ(Zero())
        "2" shouldParseTo "succ 1"
        "3" shouldParseTo "succ 2"
    }

    test("Parse iszero") {
        "iszero 0" shouldParseTo IsZero(Zero())
    }

    test("Parse If") {
        "if true then true else false" shouldParseTo If(True(), True(), False())
    }

    test("Parse Pair") {
        "{ false, 0 }" shouldParseTo Pair(False(), Zero())
    }

    test("Parse Fst") {
        "fst 0" shouldParseTo Fst(Zero())
    }

    test("Parse Snd") {
        "snd 0" shouldParseTo Snd(Zero())
    }

    test("Parse inl") {
        "inl 0 as Nat + Bool" shouldParseTo InjectLeft(Zero(), TypeSum(Nat, Bool))
        "inl 0 as Bool" shouldParseTo InjectLeft(Zero(), Bool)
    }

    test("Parse inr") {
        "inr 0 as Bool + Nat" shouldParseTo InjectRight(Zero(), TypeSum(Bool, Nat))
        "inr 0 as Bool" shouldParseTo InjectRight(Zero(), Bool)
    }

    test("Parse case") {
        val expr1 = "case inl 0 as Nat + Bool of inl x => true | inr y => false"
        val parsed1 =
            Case(InjectLeft(Zero(), TypeSum(Nat, Bool)), x, True(), y, False())
        expr1 shouldParseTo parsed1

        val expr2 = "case 0 of inl x => 0 | inr y => false"
        val parsed2 = Case(Zero(), x, Zero(), y, False())
        expr2 shouldParseTo parsed2
    }
}
