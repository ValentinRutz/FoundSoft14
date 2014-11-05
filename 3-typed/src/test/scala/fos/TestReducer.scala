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
        "fst {0, true}" shouldReduceTo "0"
    }

    test("snd test") {
        "snd {0, true}" shouldReduceTo "true"
    }

    test("iszero succ pred 0") {
        "iszero succ pred 0" shouldReduceTo "iszero succ 0"
    }

    test("case inl value") {
        "case inl 0 as Nat+Bool of inl l => iszero l | inr r => pred 0" shouldReduceTo "iszero 0"

        "case inl 0 as Nat+Bool of inl l => pred 0 | inr r => pred succ 0" shouldReduceTo "pred 0"
    }

    test("case inr value") {
        "case inr 0 as Nat+Bool of inl l => pred 0 | inr r => iszero r" shouldReduceTo "iszero 0"

        "case inr 0 as Nat+Bool of inl l => pred 0 | inr r => pred succ 0" shouldReduceTo "pred succ 0"
    }

    // CONGRUENCE

    test("if term") {
        "if iszero 0 then true else false" shouldReduceTo "if true then true else false"
        "if pred 0 then true else { 0, true }" shouldReduceTo "if 0 then true else { 0, true }"
    }

    test("iszero term") {
        "iszero pred 0" shouldReduceTo "iszero 0"
        "iszero snd {0, true}" shouldReduceTo "iszero true"
    }

    test("succ term") {
        "succ pred 0" shouldReduceTo "succ 0"
        "succ snd {0, true}" shouldReduceTo "succ true"
    }

    test("pred term") {
        "pred pred 0" shouldReduceTo "pred 0"
        "pred snd {0, true}" shouldReduceTo "pred true"
    }

    test("value term") {
        "(\\x: Nat. pred 0) pred 0" shouldReduceTo "(\\x: Nat. pred 0) 0"
        "(\\x: Bool. pred true) pred 0" shouldReduceTo "(\\x: Bool. pred true) 0"
    }

    test("term1 term2") {
        "(\\x: Nat. (\\dummy: Nat. \\y: Nat. y) 1) 0 ((\\x: Nat->Nat. x) 0)" shouldReduceTo "(\\dummy: Nat. \\y: Nat. y) 1 ((\\x: Nat->Nat. x) 0)"
    }

    test("{ pred 0, 0 }") {
        "{ pred 0, 0 }" shouldReduceTo "{0, 0}"
    }

    test("{ 0, pred 0 }") {
        "{ 0, pred 0 }" shouldReduceTo "{0, 0}"
    }

    test("{ pred 0, pred 0 }") {
        "{ pred 0,  pred 0 }" shouldReduceTo "{0, pred 0}"
    }

    test("fst { pred 0, pred 0 }") {
        "fst { pred 0,  pred 0 }" shouldReduceTo "fst {0, pred 0}"
    }

    test("snd { pred 0, pred 0 }") {
        "snd { pred 0,  pred 0 }" shouldReduceTo "snd {0, pred 0}"
    }

    val values = Seq("true", "false", "succ succ 0", """\x:Bool.x""",
        "{0,true}", "{{0, true}, succ 1}", "inl 0 as Nat+Nat", "inr 0 as Nat+Nat")

    values.foreach { term =>
        test(s"Value $term should not reduce further") {
            term shouldReduceTo term
        }
    }

    test("case term _") {
        "case pred 0 of inl l => pred succ 0 | inr r => succ pred 0" shouldReduceTo "case 0 of inl l => pred succ 0 | inr r => succ pred 0"
    }

    test("inl term") {
        "inl pred 0 as Nat+Nat" shouldReduceTo "inl 0 as Nat+Nat"
    }

    test("inr term") {
        "inr pred 0 as Nat+Nat" shouldReduceTo "inr 0 as Nat+Nat"
    }

}
