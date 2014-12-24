import fos.FJ._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestMain extends FunSuite with Matchers {

    /**
      *   Creates a test that will make sure that the main funciton will print a
      *   certain output if fed with a certain input on stdin
      */
    def inputOutput(name: String)(input: String)(output: String) =
        test(s"Input/output should hold for : $name") {

            val result = consoleFromString(input) {
                consoleToString {
                    FJ.main(Array())
                }
            }

            result should equal(output)
        }

    val baseProgram = """
        class A extends Object {
            A() { super(); }
        } 
        
        class B extends Object {
            B() { super(); }
        }

        class Pair extends Object {
            Object fst;
            Object snd;
            Pair(Object fst, Object snd){
                super(); this.fst=fst; this.snd=snd;
            }
            Pair setfst(Object newfst){
                return new Pair(newfst, this.snd);
            }
        }
        """

    inputOutput("Simple example from pdf") {
        baseProgram + "new Pair(new A(), new B()).setfst(new B())"
    } {
        """TYPE EXPR: Pair
          |EVALUATE TO: new Pair(new B(), new B())""".stripMargin
    }

    //    inputOutput("Simple cast example from pdf") {
    //        baseProgram + "((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd"
    //    } {
    //        """TYPE EXPR: Pair
    //          |EVALUATE TO: new B()""".stripMargin
    //    }

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
