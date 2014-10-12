import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestNormalOrder extends FunSuite with Matchers with LambdaTest {

    override val reducer: Term => Term = reduceNormalOrder

    test("Substitution simple") {
        "(\\y. y) x" shouldReduceTo "x"
    }

    test("""((\x.x) a) ((\y.y) b)""") {
        """((\x.x) a) ((\y.y) b)""".fullReduce("a b")(reducer)
    }
}
