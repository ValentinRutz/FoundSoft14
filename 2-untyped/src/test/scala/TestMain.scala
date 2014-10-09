import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestMain extends FunSuite with Matchers {
    test("Example from lab statement") {

        val input = """\y. ((\x.x) y)"""
        val output = """normal order: 
                        |(\y.(\x.x) y)
                        |(\y.y)
                        |call-by-value: 
                        |(\y.(\x.x) y)
                        |""".stripMargin

        val result = consoleFromString(input) {
            consoleToString {
                Untyped.main(Array())
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
