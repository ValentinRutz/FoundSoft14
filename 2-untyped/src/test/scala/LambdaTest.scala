import fos.Untyped._
import fos._
import org.scalatest.Matchers
import scala.annotation.tailrec

trait LambdaTest {
    self: Matchers =>

    val reducer: Term => Term = identity

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    def parse(in: String): Term = {
        val tokens = new lexical.Scanner(in)
        phrase(Term)(tokens) match {
            case Success(result, _) => result
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    implicit class TestingString(input: String) {

        def parse(in: String): Term = {
            val tokens = new lexical.Scanner(in)
            phrase(Term)(tokens) match {
                case Success(result, _) => result
                case Failure(msg, _) => fail(msg)
                case _ => fail("Error")
            }
        }

        def eval(reduce: Term => Term)(t: Term): Term = try {
            val next = reduce(t)
            eval(reduce)(next)
        } catch {
            case NoRuleApplies(term) => t
        }

        def fullReduce(expectedTree: Term)(reduceFun: Term => Term): Unit = {
            eval(reduceFun)(parse(input)) shouldBe expectedTree
        }

        def fullReduce(expectedTree: String)(reduceFun: Term => Term): Unit = {
            fullReduce(parse(expectedTree))(reduceFun)
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
