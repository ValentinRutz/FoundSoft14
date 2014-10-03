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

    object freshName {
        var counter = 0
        def apply(name: String): String = {
            counter = counter + 1
            name + counter
        }

        def apply(variable: Variable): Variable = {
            val Variable(name) = variable
            Variable(freshName(name))
        }
    }
    /**
      * Free variables extraction
      *
      * @param t the term to compute
      * @return the set of free variables in t
      */
    def freeVars(t: Term): Set[Variable] = t match {
        case x @ Variable(_) => Set(x)
        case Abstraction(x, t1) => freeVars(t1) - x
        case Application(t1, t2) => freeVars(t1) ++ freeVars(t2)
    }

    /**
      * Alpha-conversion
      * Only renames abstraction since they are the only
      * way to bound variable names
      *
      */
    def alpha(tree: Abstraction): Abstraction = {
        val Abstraction(variable, body) = tree
        val freshVar = freshName(variable)
        Abstraction(freshVar, subst(body)(variable.name, freshVar))
    }

    /**
      * Substitution method
      * substitute and rename variables if necessary to avoid free variable capture
      *
      * @param tree the tree to substitute
      * @param x the name of variable to substitute
      * @param s the term to substitute variable with
      * @return a new corresponding tree with substitution applied
      */
    def subst(tree: Term)(implicit x: String, s: Term): Term = tree match {
        case Variable(name) if (name == x) => s
        case Application(fun, arg) => Application(subst(fun), subst(arg))
        case a @ Abstraction(param, _) if (param.name == x) => a
        case Abstraction(param, body) if (!freeVars(s).contains(param)) => {
            Abstraction(param, subst(body))
        }
        case a @ Abstraction(_, _) => {
            // param.name != x, freeVars(s) contain param
            subst(alpha(a))
        }
        case _ => tree
    }

    /**
      * Normal order (leftmost, outermost redex first).
      *
      *  @param t the initial term
      *  @return  the reduced term
      */
    def reduceNormalOrder(t: Term): Term = t match {
        // ... To complete ...
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
