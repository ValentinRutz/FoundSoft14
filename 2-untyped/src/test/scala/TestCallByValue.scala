import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestCallByValue extends FunSuite with Matchers with LambdaTest {

    override val reducer: Term => Term = reduceCallByValue

}
