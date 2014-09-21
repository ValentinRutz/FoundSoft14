import fos.Arithmetic._
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Damien on 21/09/14.
  */
class TestBigStep extends FunSuite with Matchers {
    test("Example 1 from lab statement") {
        val input = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)
        val output = True

        eval(input) should be(output)
    }

    test("Example 2 from lab statement") {
        val input = Pred(Succ(Succ(Succ(False))))
        val output = Succ(False)

        val StuckTermException(term) = intercept[StuckTermException] {
            eval(input)
        }

        assert(term == output)
    }
}
