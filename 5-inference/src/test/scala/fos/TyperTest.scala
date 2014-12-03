package fos

class Typer extends TestSuite {
    val terms = Map(
        """\x.if x then x else x""" -> "Bool->Bool",
        """(\y.\x.if x then x else x) succ 0""" -> "Bool->Bool")

    for ((term, tpe) <- terms) {
        testTypeOf(term, tpe)
    }
}
