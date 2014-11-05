import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TypeErrorsCheck extends FunSuite with Matchers {

    def parse[T](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    val misTyped = Seq("succ true", "if false then 0 else true",
        "if 0 then true else true", "fst succ true")

    misTyped.foreach { term =>
        test(s"typeof($term) should throw an exception not containing $$") {
            val thrown = the[TypeError] thrownBy typeof(parse(Term)(term))
            thrown.msg should not contain '$'
        }
    }
}
