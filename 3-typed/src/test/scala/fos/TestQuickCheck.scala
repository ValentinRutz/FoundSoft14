import fos.SimplyTyped._
import fos.Value._
import fos._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.{ Matchers, FunSuite }

// TODO: Add Abstraction, Application, Pairs, Fst, Snd to the definitions
class TestQuickCheck extends Properties("Term") {

    def intLeafs: Gen[Term] = Zero

    def boolLeafs: Gen[Term] = Gen.oneOf(True, False)

    def succPredNodes(max: Int): Gen[Term] = for {
        t1 <- intTerms(max - 1)
        term <- Gen.oneOf(Succ(t1), Pred(t1))
    } yield term

    def isZeroNodes(max: Int): Gen[Term] = for {
        t1 <- intTerms(max - 1)
    } yield IsZero(t1)

    def intIfNodes(max: Int): Gen[Term] = for {
        b <- boolTerms(max - 1)
        t1 <- intTerms(max - 1)
        t2 <- intTerms(max - 1)
    } yield If(b, t1, t2)

    def boolIfNodes(max: Int): Gen[Term] = for {
        b <- boolTerms(max - 1)
        t1 <- boolTerms(max - 1)
        t2 <- boolTerms(max - 1)
    } yield If(b, t1, t2)

    def boolTerms(max: Int): Gen[Term] =
        if (max <= 1) boolLeafs
        else Gen.oneOf(boolLeafs, isZeroNodes(max), boolIfNodes(max))

    def intTerms(max: Int): Gen[Term] =
        if (max <= 1) intLeafs
        else Gen.oneOf(intLeafs, succPredNodes(max), intIfNodes(max))

    def typedTerms(max: Int): Gen[Term] = Gen.oneOf(boolTerms(max), intTerms(max))

    property("typedTerms reduce == value") = forAll(typedTerms(12)) { (term: Term) =>
        val res1 = path(term, reduce)
        isValue(res1.last)
    }

    def isValue(t: Term): Boolean = t match {
        case Value(v) => true
        case _ => false
    }
}
