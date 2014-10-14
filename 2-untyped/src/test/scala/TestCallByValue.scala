import fos.Untyped._
import fos._
import org.scalatest.{ Matchers, FunSuite }

class TestCallByValue extends FunSuite with Matchers with LambdaTest {

    override val reducer: Term => Term = reduceCallByValue

    def normalForm(s: String) = parse(s).toString

    val tru = normalForm("\\t. \\f. t")
    val fls = normalForm("\\t. \\f. t")

    val and = normalForm(s"\\c1. \\c2. c1 c2 $fls")
    val or = normalForm(s"\\c1. \\c2. c1 $tru c2")
    val nt = normalForm(s"\\c. c $fls $tru")

    test("and tru tru") {
        s"$and $tru $tru".fullReduce(tru)(reducer)
    }

    test("and tru fls") {
        s"$and $tru $fls".fullReduce(fls)(reducer)
    }

    test("and fls tru") {
        s"$and $fls $tru".fullReduce(fls)(reducer)
    }

    test("and fls fls") {
        s"$and $fls $fls".fullReduce(fls)(reducer)
    }

    test("or tru tru") {
        s"$or $tru $tru".fullReduce(tru)(reducer)
    }

    test("or tru fls") {
        s"$or $tru $fls".fullReduce(tru)(reducer)
    }

    test("or fls tru") {
        s"$or $fls $tru".fullReduce(tru)(reducer)
    }

    test("or fls fls") {
        s"$or $fls $fls".fullReduce(fls)(reducer)
    }

    test("nt fls") {
        s"$nt $fls".fullReduce(tru)(reducer)
    }

    test("nt tru") {
        s"$nt $tru".fullReduce(fls)(reducer)
    }

    test("""((\x.x) a) ((\y.y) b)""") {
        val term = normalForm("""((\x.x) a) ((\y.y) b)""")
        term.fullReduce(term)(reducer)
        term shouldReduceTo "a" // this is dependent on what we put in NoRuleApplies exception
    }
}
