import fos.SimplyTyped._
import fos._
import org.scalatest.Matchers
import scala.annotation.tailrec

trait LambdaTest {
    self: Matchers =>

    val reducer: Term => Term = identity

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")

    val Nat = TypeNat()
    val Bool = TypeBool()

    def parse[T](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(result, _) => result
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    val parseType: String => Type = parse(Type)
    val parseTerm: String => Term = parse(Term)

    implicit class TestingTermString(input: String) {

        def eval(reduce: Term => Term)(t: Term): Term = try {
            val next = reduce(t)
            eval(reduce)(next)
        } catch {
            case NoRuleApplies(term) => t
        }

        def fullReduce(expectedTree: Term)(reduceFun: Term => Term): Unit = {
            val tree = parseTerm(input)
            typeof(tree)
            eval(reduceFun)(tree) shouldBe expectedTree
        }

        def fullReduce(expectedTree: String)(reduceFun: Term => Term): Unit = {
            fullReduce(parseTerm(expectedTree))(reduceFun)
        }

        def test(expectedTree: Term)(reduceFun: Term => Term): Unit = {
            val term = parseTerm(input)
            val result = try {
                val res = reduceFun(term)
                res
            } catch {
                case NoRuleApplies(_) => term
                case t: TypeError => term
            }
            result should be(expectedTree)
        }

        def shouldParseTo(expectedTree: Term): Unit = {
            test(expectedTree)(identity)
        }

        def shouldParseTo(equivalentForm: String): Unit = {
            test(parseTerm(equivalentForm))(identity)
        }

        def shouldReduceTo(expectedTree: Term): Unit = {
            test(expectedTree)(reducer)
        }
        def shouldReduceTo(expectedForm: String): Unit = {
            test(parseTerm(expectedForm))(reducer)
        }
    }

    implicit class TestingTypeString(input: String) {
        def shouldParseToType(expectedType: Type): Unit = {
            parseType(input) should be(expectedType)
        }
        def shouldParseToType(expectedType: String): Unit = {
            parseType(input) should be(parseType(expectedType))
        }
    }
}
