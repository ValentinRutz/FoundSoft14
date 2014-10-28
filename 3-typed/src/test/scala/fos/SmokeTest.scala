import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class SmokeTest extends FunSuite with Matchers {

    def parse(in: String): Term = {
        val tokens = new lexical.Scanner(in)
        phrase(Term)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    /**
      * Creates a test to check that parsing a string and
      * returning its toString returns the same value
      */
    def parseAndToString(term: String) = test(s"parse($term).toString == $term") {
        parse(term).toString should equal(term)
    }

    val simpleTerms = Seq("true", "false", "if true then true else true", "0",
        "pred 0", "succ 0", "iszero 0", "x", """\x:Nat.x""", "t t")

    simpleTerms.foreach { term =>
        parseAndToString(term)
    }
}
