package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
  * This object implements a parser and evaluator for the
  *  simply typed lambda calculus found in Chapter 9 of
  *  the TAPL book.
  */
object SimplyTyped extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
        "pred", "iszero", "let", "in", "fst", "snd")

    /**
      * Term     ::= SimpleTerm { SimpleTerm }
      */
    def Term: Parser[Term] = positioned(
        failure("illegal start of term"))

    /**
      * SimpleTerm ::= "true"
      *               | "false"
      *               | number
      *               | "succ" Term
      *               | "pred" Term
      *               | "iszero" Term
      *               | "if" Term "then" Term "else" Term
      *               | ident
      *               | "\" ident ":" Type "." Term
      *               | "(" Term ")"
      *               | "let" ident ":" Type "=" Term "in" Term
      *               | "{" Term "," Term "}"
      *               | "fst" Term
      *               | "snd" Term
      */
    def SimpleTerm: Parser[Term] = positioned(
        "true" ^^^ True
            | "false" ^^^ False
            | ("if" ~> Term) ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ { case c ~ t ~ e => If(c, t, e) }
            | "succ" ~> Term ^^ Succ
            | "pred" ~> Term ^^ Pred
            | "iszero" ~> Term ^^ IsZero
            | numericLit ^^ { case e => Iterator.iterate[Term](Zero)(Succ).toStream(e.toInt) }
            | ("\\" ~> ident) ~ (":" ~> Type) ~ ("." ~> Term) ^^ {
                case param ~ typ ~ body => Abstraction(Variable(param), typ, body)
            }
            | "(" ~> Term <~ ")"
            //   ... To complete ... with let and pair
            | failure("illegal start of simple term"))

    /**
      * Type       ::= SimpleType [ "->" Type ]
      */
    def Type: Parser[Type] = positioned(
        SimpleType ~ opt("->" ~> Type) ^^ {
            case from ~ Some(to) =>
                TypeFun(from, to)
            case typ ~ None => typ
        }
            | failure("illegal start of type"))

    def SimpleType: Parser[Type] = positioned(
        "Bool" ^^^ TypeBool
            | "Nat" ^^^ TypeNat
            | "(" ~> Type <~ ")"
            | failure("illegal start of type"))

    /** Thrown when no reduction rule applies to the given term. */
    case class NoRuleApplies(t: Term) extends Exception(t.toString)

    /** Print an error message, together with the position where it occured. */
    case class TypeError(pos: Position, msg: String) extends Exception(msg) {
        override def toString =
            msg + "\n" + pos.longString
    }

    /** The context is a list of variable names paired with their type. */
    type Context = List[(String, Type)]

    /** Is the given term a numeric value? */
    def isNumericVal(t: Term): Boolean = t match {
        case Zero => true
        case Succ(t) => isNumericVal(t)
        case _ => false
    }

    /** Is the given term a value? */
    def isValue(t: Term): Boolean = t match {
        case True | False | Abstraction(_, _, _) => true
        case nv if (isNumericVal(nv)) => true
        case _ => false
    }

    object freshName {
        var names: Set[String] = Set.empty
        def namesOf(term: Term): Set[String] = term match {
            case Variable(name) => Set(name)
            case Abstraction(param, typ, body) => namesOf(body) + param.name
            case Application(fun, arg) => namesOf(fun) ++ namesOf(arg)
            case Succ(term) => namesOf(term)
            case Pred(term) => namesOf(term)
            case IsZero(term) => namesOf(term)
            case If(c, t, e) => namesOf(c) ++ namesOf(t) ++ namesOf(e)
        }
        def addNames(term: Term): Unit = {
            names = names ++ namesOf(term)
        }
        val Versioned = """([^\$]+)\$(\d+)""".r
        var counter = 0
        def apply(name: String): String = {
            val realName = name match {
                case Versioned(n, v) => n
                case _ => name
            }
            counter = counter + 1
            val newName = (realName + "$" + counter)
            if (names contains newName) freshName(realName)
            else {
                names = names + newName
                newName
            }
        }

        def apply(variable: Variable): Variable = {
            val Variable(name) = variable
            Variable(freshName(name))
        }
    }

    /** Call by value reducer. */
    def reduce(t: Term): Term = t match {
        case _ =>
            throw NoRuleApplies(t)
    }

    /**
      * Returns the type of the given term <code>t</code>.
      *
      *  @param ctx the initial context
      *  @param t   the given term
      *  @return    the computed type
      */
    def typeof(ctx: Context, t: Term): Type = t match {
        case True | False =>
            TypeBool
        //   ... To complete ... 
    }

    /**
      * Returns a stream of terms, each being one step of reduction.
      *
      *  @param t      the initial term
      *  @param reduce the evaluation strategy used for reduction.
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
                try {
                    println("typed: " + typeof(Nil, trees))
                    for (t <- path(trees, reduce))
                        println(t)
                } catch {
                    case tperror: TypeError => println(tperror.toString)
                }
            case e =>
                println(e)
        }
    }
}
