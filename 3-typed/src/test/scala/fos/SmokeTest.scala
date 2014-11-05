import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class SmokeTest extends FunSuite with Matchers {

    def parse[T](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    val simpleTerms = Seq("true", "false", "if true then true else true", "0",
        "pred 0", "succ 0", "iszero 0", "x", """\x:Nat.x""", "t t", "{0, 0}",
        "fst {0, 0}", "snd {0, 0}", "{{0, true}, 0}", "inl 0 as Nat+Bool",
        "inr false as Nat+Bool", "case 0 of inl a => a | inr b => succ b",
        """fix \x:Nat.0""", "fix false")

    simpleTerms.foreach { term =>
        test(s"parse($term).toString == $term") {
            parse(Term)(term).toString should equal(term)
        }
    }

    val simpleTypes = Seq("Nat", "Bool", "Nat*Nat", "Nat->Bool",
        "(Nat->Nat)->Bool", "(Nat*Nat)*Nat", "Nat*(Bool->Nat)",
        "(Bool->Nat)*Bool", "(Bool->Bool)*(Nat->Nat)", "Bool+Nat",
        "(Bool+Bool)*Bool", "(Nat*Nat)+Bool")

    simpleTypes.foreach { typ =>
        test(s"parse($typ).toString == $typ") {
            parse(Type)(typ).toString should equal(typ)
        }
    }

    test("Let should translate correctly") {
        parse(Term)("let x : Nat = 0 in x").toString should equal("""(\x:Nat.x) 0""")
    }

    test("Letrec should translate to equivalent let expr") {
        val letrec = parse(Term)("letrec x : Nat = t1 in t2").toString
        val let = parse(Term)("""let x : Nat = fix (\x:Nat.t1) in t2""").toString
        letrec should equal(let)
    }

    test("Letrec should translate correctly") {
        val letrec = parse(Term)("letrec a : Bool = x in y").toString
        val desugared = """(\a:Bool.y) (fix \a:Bool.x)"""
        letrec should equal(desugared)
    }
}
