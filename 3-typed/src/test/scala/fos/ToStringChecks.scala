import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class ToStringChecks extends FunSuite with Matchers {

    def parse[T](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    val terms = Seq("true", "false", "if true then true else true", "0",
        "pred 0", "succ 0", "iszero 0", "x", """\x:Nat.x""", "t t", "{0, 0}",
        "fst {0, 0}", "snd {0, 0}", "{{0, true}, 0}", "(((((((0)))))))",
        """\x:Nat.x y x""", """(\x:Nat.x) y x""",
        "(case 0 of inl y => t | inr x => t) a")

    terms.foreach { term =>
        test(s"toString should be well behaved on $term") {
            val parsedTerm = parse(Term)(term)
            parse(Term)(parsedTerm.toString) should equal(parsedTerm)
        }
    }

    val types = Seq("Nat", "Bool", "Nat*Nat", "Nat->Bool",
        "(Nat->Nat)->Bool", "(Nat*Nat)*Nat", "Nat*(Bool->Nat)",
        "(Bool->Nat)*Bool", "(Bool->Bool)*(Nat->Nat)")

    types.foreach { typ =>
        test(s"toString should be well behaved on $typ") {
            val parsedTyp = parse(Type)(typ)
            parse(Type)(parsedTyp.toString) should equal(parsedTyp)
        }
    }
}
