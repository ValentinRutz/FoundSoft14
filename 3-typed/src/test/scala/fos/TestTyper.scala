import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestTyper extends FunSuite with Matchers with LambdaTest {

    test("Infinite loop for type error") {
        an[TypeError] should be thrownBy typeof(parse(Term)("iszero true"))
    }

    val boolTyped = Seq("true", "false", "iszero 0",
        "if true then true else true", "let x : Bool = true in x",
        "fst {true, 0}", "snd {0, false}")

    boolTyped foreach { term =>
        test(s"$term should have type Bool") {
            typeof(parse(Term)(term)) should be(TypeBool)
        }
    }
}
