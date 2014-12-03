package fos

class Typer extends TestSuite {
    val terms = Map(
        """\x.if x then x else x""" -> "Bool->Bool",
        """(\y.\x.if x then x else x) succ 0""" -> "Bool->Bool",
        """\x. let y = succ x in y""" -> "Nat->Nat",
        """\x. let x = x in succ x""" -> "Nat->Nat",
        """\x. x (let y = x in y 0)""" -> "(Nat->Nat)->Nat",
        """\x. (\y. succ y) x""" -> "Nat->Nat")

    for ((term, tpe) <- terms) {
        testTypeOf(term, tpe)
    }

    val badTerms = List(
        """succ true""")

    for (t <- badTerms) testBadType(t)
}
