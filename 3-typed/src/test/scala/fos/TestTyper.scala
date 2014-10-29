import fos.SimplyTyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestTyper extends FunSuite with Matchers with LambdaTest {

    test("Infinite loop for type error") {
        an[TypeError] should be thrownBy typeof(Nil, parse(Term)("iszero true"))
    }
}
