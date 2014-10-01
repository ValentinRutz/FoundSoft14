import fos.Arithmetic.eval
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

        output.isValue should be(false)
        eval(input) should be(output)
    }

    test("Simple tests") {
        val singleLevel = Map(
            If(True, True, False) -> True,
            If(False, True, False) -> False,
            IsZero(Zero) -> True,
            IsZero(Succ(Zero)) -> False,
            Pred(Zero) -> Zero,
            Pred(Succ(Zero)) -> Zero)

        for ((input, output) <- singleLevel) {
            eval(input) should be(output)
        }

        val congruences = Map(
            If(IsZero(Zero), True, False) -> True,
            IsZero(Pred(Succ(Zero))) -> True,
            Pred(Pred(Zero)) -> Zero,
            Succ(Pred(Zero)) -> Succ(Zero),
            Pred(Succ(Zero)) -> Zero)

        for ((input, output) <- congruences) {
            eval(input) should be(output)
        }

    }

    test("Stuck terms") {
        val simpleStuckTerms = Seq(
            If(Zero, Pred(Succ(Zero)), Succ(Pred(Zero))),
            IsZero(False),
            Pred(False),
            Succ(True))

        for (term <- simpleStuckTerms) {
            term.isValue should be(false)
            eval(term) should be(term)
        }

        val stuckTerms = Map(
            Succ(IsZero(False)) -> IsZero(False),
            If(False, Succ(False), Pred(True)) -> Pred(True),
            If(True, Succ(False), Pred(True)) -> Succ(False),
            IsZero(Succ(IsZero(Zero))) -> Succ(IsZero(Zero)),
            Succ(IsZero(IsZero(Zero))) -> IsZero(IsZero(Zero)))

        for ((term, stuck) <- stuckTerms) {
            stuck.isValue should be(false)
            eval(term) should be(stuck)
        }
    }

}
