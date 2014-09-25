import fos.Arithmetic.reduce
import fos._
import org.scalatest.{ Matchers, FunSuite }

/**
  * Created by Valentin on 18/09/14.
  */
class TestOneStep extends FunSuite with Matchers {
    test("Example 1 from lab statement") {
        val steps: Seq[Term] = Seq(
            If(IsZero(Pred(Pred(Succ(Succ(Zero))))), If(IsZero(Zero), True, False), False),
            If(IsZero(Pred(Succ(Zero))), If(IsZero(Zero), True, False), False),
            If(IsZero(Zero), If(IsZero(Zero), True, False), False),
            If(True, If(IsZero(Zero), True, False), False),
            If(IsZero(Zero), True, False),
            If(True, True, False),
            True)

        for (Seq(input, output) <- steps.sliding(2)) {
            reduce(input) should be(output)
        }
    }

    test("Example 2 from lab statement") {
        val input = Pred(Succ(Succ(Succ(False))))
        reduce(input) should be(input)
    }

    test("Regression for IsZero(Succ(Pred(Zero)))") {
        reduce(IsZero(Succ(Pred(Zero)))) should be(IsZero(Succ(Zero)))
    }

    test("Simple tests") {
        val computations = Map(
            If(True, True, False) -> True,
            If(False, True, False) -> False,
            IsZero(Zero) -> True,
            IsZero(Succ(Zero)) -> False,
            Pred(Zero) -> Zero,
            Pred(Succ(Zero)) -> Zero)

        for ((input, output) <- computations) {
            reduce(input) should be(output)
        }

        val congruences = Map(
            If(IsZero(Zero), True, False) -> If(True, True, False),
            IsZero(Pred(Succ(Zero))) -> IsZero(Zero),
            Pred(Pred(Zero)) -> Pred(Zero),
            Succ(Pred(Zero)) -> Succ(Zero))

        for ((input, output) <- congruences) {
            reduce(input) should be(output)
        }
    }

    test("Stuck terms") {
        val stuckTerms = Seq(
            If(Zero, Pred(Succ(Zero)), Succ(Pred(Zero))),
            IsZero(False),
            Pred(False),
            Succ(True))

        for (term <- stuckTerms) {
            reduce(term) should be(term)
        }
    }
}
