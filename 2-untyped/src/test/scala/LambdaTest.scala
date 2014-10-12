import fos.Untyped._
import fos._
import org.scalatest.Matchers

trait LambdaTest {
    self: Matchers =>

    val reducer: Term => Term = identity

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")

    implicit class TestingString(input: String) {

        def parse(in: String): Term = {
            val tokens = new lexical.Scanner(in)
            phrase(Term)(tokens) match {
                case Success(result, _) => result
                case Failure(msg, _) => fail(msg)
                case _ => fail("Error")
            }
        }

        def test(expectedTree: Term)(reduceFun: Term => Term): Unit = {
            val result = try {
                reduceFun(parse(input))
            } catch {
                case NoRuleApplies(term) => term
            }
            result should be(expectedTree)
        }

        def shouldParseTo(expectedTree: Term): Unit = {
            test(expectedTree)(identity)
        }

        def shouldParseTo(equivalentForm: String): Unit = {
            test(parse(equivalentForm))(identity)
        }

        def shouldReduceTo(expectedTree: Term): Unit = {
            test(expectedTree)(reducer)
        }
        def shouldReduceTo(expectedForm: String): Unit = {
            test(parse(expectedForm))(reducer)
        }
    }
}
