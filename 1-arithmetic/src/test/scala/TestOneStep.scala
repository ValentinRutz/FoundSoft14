import fos.Arithmetic._
import fos._
import org.scalatest.FunSuite

/**
  * Created by Valentin on 18/09/14.
  */
class TestOneStep extends FunSuite {
    test("First example from statement") {
        val input = If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False)
        val output = If(IsZero(Pred(Succ(Zero))), If(IsZero(Zero), True, False), False)
        assert(oneStepEvaluator(input) == output)
    }

    test("Bad example with Big Step") {
        val input = Pred(Succ(Succ(Succ(False))))
        val output = Succ(Succ(False))
        assert(oneStepEvaluator(input) == output)
    }
}
