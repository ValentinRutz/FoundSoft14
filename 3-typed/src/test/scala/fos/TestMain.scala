import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestMain extends FunSuite with Matchers {

    def inputOutput(name: String)(input: String)(output: String) =
        test(s"Input/output should hold for : $name") {

            val result = consoleFromString(input) {
                consoleToString {
                    SimplyTyped.main(Array())
                }
            }

            result should equal(output)
        }

    inputOutput("Example1 from lab statement") {
        """(\x:Nat->Bool. (\y:Nat.(x y))) (\x:Nat.(iszero x)) 0"""
    } {
        """typed: Bool
          |(\x:Nat->Bool.(\y:Nat.x y)) (\x:Nat.iszero x) 0
          |(\y:Nat.(\x:Nat.iszero x) y) 0
          |(\x:Nat.iszero x) 0
          |iszero 0
          |true
          |""".stripMargin
    }

    inputOutput("Example2 from lab statement") {
        """(\x:Nat.x) true"""
    } {
        """parameter type mismatch: expected Nat, found Bool
          |(\x:Nat.x) true
          |""".stripMargin
    }

    inputOutput("Example3 from lab statement") {
        """(\x:Nat.snd x) 1"""
    } {
        """pair type expected but Nat found
          |(\x:Nat.snd x) 1
          |""".stripMargin
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
