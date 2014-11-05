import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestTypeParser extends FunSuite with Matchers with LambdaTest {
    test("Bool") { "Bool" shouldParseToType Bool }
    test("Nat") { "Nat" shouldParseToType Nat }
    test("Nat -> Bool") { "Nat -> Bool" shouldParseToType TypeFun(Nat, Bool) }
    test("Nat * Bool") { "Nat * Bool" shouldParseToType TypePair(Nat, Bool) }
    test("Nat + Bool") { "Nat + Bool" shouldParseToType TypeSum(Nat, Bool) }
    test("(Nat)") { "(Nat)" shouldParseToType "Nat" }
    test("parenthesis") {
        "(Nat -> Nat) -> Nat" shouldParseToType TypeFun(TypeFun(Nat, Nat), Nat)
        "Nat -> (Nat -> Nat)" shouldParseToType TypeFun(Nat, TypeFun(Nat, Nat))
    }
    test("-> right associative") {
        "Nat -> Nat -> Nat" shouldParseToType "Nat -> (Nat -> Nat)"
    }
    test("* right associative") {
        "Nat * Nat * Nat" shouldParseToType "Nat * (Nat * Nat)"
    }
    test("+ righ associative") {
        "Nat + Nat + Nat" shouldParseToType "Nat + (Nat + Nat)"
    }
    test("* precedence over ->") {
        "Nat -> Nat * Nat" shouldParseToType "Nat -> (Nat * Nat)"
        "Nat * Nat -> Nat" shouldParseToType "(Nat * Nat) -> Nat"
    }

    test("+ precedence over ->") {
        "Nat -> Nat + Nat" shouldParseToType "Nat -> (Nat + Nat)"
        "Nat + Nat -> Nat" shouldParseToType "(Nat + Nat) -> Nat"
    }

    test("+ same priority as *") {
        "Nat * Nat + Nat" shouldParseToType "Nat * (Nat + Nat)"
        "Nat + Nat * Nat" shouldParseToType "Nat + (Nat * Nat)"

    }
}
