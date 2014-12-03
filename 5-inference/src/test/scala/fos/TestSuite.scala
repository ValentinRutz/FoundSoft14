import fos.Infer._
import fos._
import org.scalatest.{ Matchers, FunSuite }

trait TestSuite extends FunSuite with Matchers {

    def parseOpt[T <: Term](parser: Parser[T])(in: String): Option[T] = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => Some(res)
            case Failure(msg, _) => None
            case _ => None
        }
    }

    def parse[T <: Term](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    /**
      * Test that parsing in returns something
      */
    def parseSuccess(in: String) = {
        test(s"parse($in) should succeed") {
            val termOpt = parseOpt(Infer.Term)(in) orElse parseOpt(Infer.Type)(in)
            termOpt.isDefined should be(true)
        }
    }

    /**
      * Test that parsing bad fails
      */
    def parseError(bad: String) = {
        test(s"parse($bad) should fail") {
            parseOpt(Infer.Type)(bad) should be(None)
            parseOpt(Infer.Term)(bad) should be(None)
        }
    }

    /**
      * Test that parsing in returns out
      */
    def testParse[T <: Term](in: String, out: T) = {
        val parser = if (out.isInstanceOf[TypeTree]) Infer.Type else Infer.Term
        test(s"parse($in) == $out") {
            parse(parser)(in) should equal(out)
        }
    }

    /**
      * Test that calling toString on a term returns the correct value
      */
    def testToString(in: Term, out: String) = {
        test(s"($in).toString == out") {
            in.toString should equal(out)
        }
    }

    /**
      * Test that parsing and calling toString returns the same term
      */
    def parseToString(in: String) = {
        test(s"parse($in).toString == $in") {
            val term = parseOpt(Infer.Term)(in) getOrElse parse(Infer.Type)(in)
            term.toString should equal(in)
        }
    }
}
