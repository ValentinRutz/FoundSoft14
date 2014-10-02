package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
  * This object implements a parser and evaluator for the
  *  untyped lambda calculus found in Chapter 5 of
  *  the TAPL book.
  */
object Untyped extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "\\", ".")
    import lexical.Identifier

    /**
      * Term     ::= AbsOrVar { AbsOrVar }
      */
    def Term: Parser[Term] = (
        rep(Abs | Var | Par) ^^ {
            _ reduceLeft (Application(_, _))
        }
        | failure("illegal start of term"))

    def Abs: Parser[Abstraction] = (
        ("\\" ~> ident) ~ ("." ~> Term) ^^ {
            case param ~ body => Abstraction(Variable(param), body)
        })

    def Var: Parser[Variable] = (
        ident ^^ {
            case ident => Variable(ident)
        })

    def Par: Parser[Term] = {
        "(" ~> Term <~ ")"
    }

    /** Term 't' does not match any reduction rule. */
    case class NoRuleApplies(t: Term) extends Exception(t.toString)

    /**
      * Normal order (leftmost, outermost redex first).
      *
      *  @param t the initial term
      *  @return  the reduced term
      */
    def reduceNormalOrder(t: Term): Term = t match {
        case _ => throw NoRuleApplies(t)
    }

    /** Call by value reducer. */
    def reduceCallByValue(t: Term): Term = t match {
        //   ... To complete ... 
        case _ => throw NoRuleApplies(t)
    }

    /**
      * Returns a stream of terms, each being one step of reduction.
      *
      *  @param t      the initial term
      *  @param reduce the method that reduces a term by one step.
      *  @return       the stream of terms representing the big reduction.
      */
    def path(t: Term, reduce: Term => Term): Stream[Term] =
        try {
            var t1 = reduce(t)
            Stream.cons(t, path(t1, reduce))
        } catch {
            case NoRuleApplies(_) =>
                Stream.cons(t, Stream.empty)
        }

    def main(args: Array[String]): Unit = {
        val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
        phrase(Term)(tokens) match {
            case Success(trees, _) =>
                println("normal order: ")
                for (t <- path(trees, reduceNormalOrder))
                    println(t)
                println("call-by-value: ")
                for (t <- path(trees, reduceCallByValue))
                    println(t)

            case e =>
                println(e)
        }
    }
}
