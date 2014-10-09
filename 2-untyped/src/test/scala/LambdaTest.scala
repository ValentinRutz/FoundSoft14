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
        val tokens = new lexical.Scanner(input)
        def shouldParseTo(expectedTree: Term): Unit = {
            test(expectedTree)(identity)
        }

        def shouldParseTo(equivalentForm: String): Unit = {
            test(equivalentForm)(identity)
        }

        def test(expectedTree: Term)(reduceFun: Term => Term): Unit = {
            phrase(Term)(tokens) match {
                case Success(result, _) => {
                    reduceFun(result) should be(expectedTree)
                }
                case Failure(msg, _) => fail(msg)
                case _ => fail("someting went really wrong")
            }
        }

        def test(expectedForm: String)(reduceFun: Term => Term): Unit = {
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

        def shouldReduceTo(expectedTree: Term): Unit = {
            test(expectedTree)(reducer)
        }
        def shouldReduceTo(expectedForm: String): Unit = {
            test(expectedForm)(reducer)
        }

        def shouldReduceNormalTo(expectedTree: Term): Unit = {
            test(expectedTree)(reduceNormalOrder)
        }
        def shouldReduceNormalTo(expectedFrom: String): Unit = {
            test(expectedFrom)(reduceNormalOrder)
        }
        def shouldReduceValueTo(expectedTree: Term): Unit = {
            test(expectedTree)(reduceCallByValue)
        }
        def shouldReduceValueTo(expectedForm: String): Unit = {
            test(expectedForm)(reduceCallByValue)
        }
    }
}
