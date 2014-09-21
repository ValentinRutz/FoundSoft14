import fos.Arithmetic._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestPrint extends FunSuite with Matchers {
    test("Example 1 from lab statement") {

        val input = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)
        val output = """If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
                       |If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
                       |If(IsZero(Zero),If(IsZero(Zero),True,False),False)
                       |If(True,If(IsZero(Zero),True,False),False)
                       |If(IsZero(Zero),True,False)
                       |If(True,True,False)
                       |True
                       |Big step: True
                       |""".stripMargin

        val result = consoleToString {
            smallStepPrint(input)
            bigStepPrint(input)
        }

        result should be(output)
    }

    test("Example 2 from lab statement") {
        val input = Pred(Succ(Succ(Succ(False))))
        val output = """Pred(Succ(Succ(Succ(False))))
                       |Stuck term: Pred(Succ(Succ(Succ(False))))
                       |Big step: Stuck term: Succ(False)
                       |""".stripMargin

        val result = consoleToString {
            smallStepPrint(input)
            bigStepPrint(input)
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
}
