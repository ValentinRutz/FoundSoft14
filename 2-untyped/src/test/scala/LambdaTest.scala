import fos.Untyped._
import fos._
import org.scalatest.Matchers

trait LambdaTest {
    self: Matchers =>

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")

    implicit class TestingString(input: String) {
        val tokens = new lexical.Scanner(input)
        def shouldParseTo(expectedTree: Term): Unit = {
            val reduceFun: Term => Term = identity
            shouldReduceTo(expectedTree)
        }

        def shouldParseTo(equivalentForm: String): Unit = {
            val reduceFun: Term => Term = identity
            shouldReduceTo(equivalentForm)
        }

        def shouldReduceTo(expectedTree: Term)(implicit reduceFun: Term => Term): Unit = {
            phrase(Term)(tokens) match {
                case Success(result, _) => {
                    reduceFun(result) should be(expectedTree)
                }
                case Failure(msg, _) => fail(msg)
                case _ => fail("someting went really wrong")
            }
        }

        def shouldReduceTo(expectedForm: String)(implicit reduceFun: Term => Term): Unit = {
            val tokens2 = new lexical.Scanner(expectedForm)
            phrase(Term)(tokens) match {
                case Success(result, _) => phrase(Term)(tokens2) match {
                    case Success(result2, _) => {
                        reduceFun(result) should be(result2)
                    }
                    case Failure(msg, _) => fail(msg)
                    case _ => fail("something went really wrong")
                }
                case Failure(msg, _) => fail(msg)
                case _ => fail("something went really wrong")
            }
        }
    }
}
