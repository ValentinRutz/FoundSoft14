import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestTyper extends FunSuite with Matchers with LambdaTest {

    test("Infinite loop for type error") {
        an[TypeError] should be thrownBy typeof(parse(Term)("iszero true"))
    }

    def expectedType(term: String, typ: String) =
        test(s"$term should have type $typ") {
            typeof(parse(Term)(term)) should be(parse(Type)(typ))
        }

    def typeError(term: String) = test(s"$term should throw a type error") {
        an[TypeError] should be thrownBy typeof(parse(Term)(term))
    }

    val boolTyped = Seq("true", "false", "iszero 0",
        "if true then true else true", "let x : Bool = true in x",
        "fst {true, 0}", "snd {0, false}")

    boolTyped.foreach(expectedType(_, "Bool"))

    val intTyped = Seq("0", "succ 5", "pred 17",
        "if true then 9 else 1", "let x : Nat = 7 in x",
        "snd {true, 0}", "fst {0, false}")

    intTyped.foreach(expectedType(_, "Nat"))

    expectedType("{0, false}", "Nat*Bool")

    expectedType("""\x : Nat.false""", "Nat->Bool")

    typeError("fst 0")
    typeError("snd true")
    typeError("x")
}
