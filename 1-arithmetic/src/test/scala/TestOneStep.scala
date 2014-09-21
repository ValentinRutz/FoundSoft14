import fos.Arithmetic._
import fos._
import org.scalatest.FunSuite

/**
  * Created by Valentin on 18/09/14.
  */
class TestOneStep extends FunSuite {
    test("Example 1 from lab statement") {
        val steps: Seq[Term] = Seq(
            If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False),
            If(IsZero(Pred(Succ(Zero))), If(IsZero(Zero), True, False), False),
            If(IsZero(Zero), If(IsZero(Zero), True, False), False),
            If(True, If(IsZero(Zero), True, False), False),
            If(True, True, False),
            True)

        for (Seq(input, output) <- steps.sliding(2)) {
            assert(reduce(input) == output)
        }
    }

    test("Example 2 from lab statement") {
        val input = Pred(Succ(Succ(Succ(False))))
        val output = Pred(Succ(Succ(Succ(False))))
        assert(reduce(input) == output)
    }
}
