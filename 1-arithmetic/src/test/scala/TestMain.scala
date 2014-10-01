import fos.Arithmetic._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestMain extends FunSuite with Matchers {
    test("Example 1 from lab statement") {

        val input = "if iszero pred pred 2 then if iszero 0 then true else false else false"
        val output = """If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
                       |If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
                       |If(IsZero(Zero),If(IsZero(Zero),True,False),False)
                       |If(True,If(IsZero(Zero),True,False),False)
                       |If(IsZero(Zero),True,False)
                       |If(True,True,False)
                       |True
                       |Big step: True
                       |""".stripMargin

        val result = consoleFromString(input) {
            consoleToString {
                Arithmetic.main(Array())
            }
        }

        result should be(output)
    }

    test("Example 2 from lab statement") {
        val input = "pred succ succ succ false"
        val output = """Pred(Succ(Succ(Succ(False))))
                       |Stuck term: Pred(Succ(Succ(Succ(False))))
                       |Big step: Stuck term: Succ(False)
                       |""".stripMargin

        val result = consoleFromString(input) {
            consoleToString {
                Arithmetic.main(Array())
            }
        }

        result should be(output)
    }

    /** Executes a block of code and redirects all the console output into a string */
    def consoleToString(block: => Unit): String = {
        val stream = new java.io.ByteArrayOutputStream()
        Console.withOut(stream) {
            block
        }

        new String(stream.toByteArray)
    }

    /** Executes a block of code and take stdin from a string */
    def consoleFromString[T](input: String)(block: => T): T = {
        /* This is not very satisfying, but using Console.withIn(new
         * java.io.StringReader(input)) seems to hang */
        val systemIn = System.in
        System.setIn(new java.io.ByteArrayInputStream(input.getBytes))
        val res = block
        System.setIn(systemIn)
        res
    }
}
