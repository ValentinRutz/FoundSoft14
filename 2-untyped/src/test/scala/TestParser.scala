import fos.Untyped._
import fos._
import org.scalatest.FunSuite

class TestParser extends FunSuite {
    test("Parser match sample imput") {

        val tokens = new lexical.Scanner("x")
        phrase(Term)(tokens) match {
            case Success(trees, _) => println(trees)
        }
    }
}
