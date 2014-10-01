package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.annotation.tailrec
import java.lang.IllegalArgumentException

/**
  * This object implements a parser and evaluator for the NB
  *  language of booleans and numbers found in Chapter 3 of
  *  the TAPL book.
  */
object Arithmetic extends StandardTokenParsers {
    lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

    import lexical.NumericLit

    /**
      * Parser for the NB language defined by the grammar below
      *
      * Expr ::= 'true'
      *  	| 'false'
      *  	| 'if' Expr 'then' Expr 'else' Expr
      *  	| '0'
      *  	| 'succ' Expr
      *  	| 'pred' Expr
      *  	| 'iszero' Expr
      */
    def Expr: Parser[Term] = (
        "true" ^^^ True
        | "false" ^^^ False
        | ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case c ~ t ~ e => If(c, t, e) }
        | "succ" ~> Expr ^^ Succ
        | "pred" ~> Expr ^^ Pred
        | "iszero" ~> Expr ^^ IsZero
        | numericLit ^^ { case e => Iterator.iterate[Term](Zero)(Succ).drop(e.toInt).next }
        | failure("illegal start of expression"))

    /**
      * "one step" evaluator
      *
      * @param tree The term to reduce
      * @return The result of one step of the evaluation
      */
    def reduce(tree: Term): Term = tree match {
        /* Computation */
        case If(True, t, e) => t
        case If(False, t, e) => e
        case IsZero(Zero) => True
        case IsZero(Succ(nv)) if nv.isNumericValue => False
        case Pred(Zero) => Zero
        case Pred(Succ(nv)) if nv.isNumericValue => nv

        /* Congruence */
        case If(c, t, e) => If(reduce(c), t, e)
        case IsZero(s) => IsZero(reduce(s))
        case Pred(s) => Pred(reduce(s))
        case Succ(s) => Succ(reduce(s))

        /* Values */
        case v if v.isValue => v

        /* Error */
        case error => throw new IllegalArgumentException("Unexpected expression: " + error)
    }

    /**
      * "big step" evaluator
      *
      * @param tree the term to evaluate
      * @return  the result of the evaluator
      */
    def eval(tree: Term): Term = tree match {
        /* B-VALUE */
        case v if v.isValue => v

        /* B-IFTRUE, B-IFFALSE */
        case If(c, t, e) => eval(c) match {
            case True => eval(t)
            case False => eval(e)
            case s if s.isValue => tree
            case s => s
        }

        /* B-SUCC */
        case Succ(t) => eval(t) match {
            case nv if nv.isNumericValue => Succ(nv)
            case s if s.isValue => tree
            case s => s
        }

        /* B-PREDZERO, B-PREDSUCC */
        case Pred(t) => eval(t) match {
            case Zero => Zero
            case Succ(nv) if nv.isNumericValue => nv
            case s if s.isValue => tree
            case s => s
        }

        /* B-ISZEROZERO, B-ISZEROSUCC */
        case IsZero(t) => eval(t) match {
            case Zero => True
            case Succ(nv) if nv.isNumericValue => False
            case s if s.isValue => tree
            case s => s
        }

        /* Error */
        case error => throw new IllegalArgumentException("Unexpected expression: " + error)
    }

    def main(args: Array[String]): Unit = {
        val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
        phrase(Expr)(tokens) match {
            case Success(trees, _) => {
                // small step
                smallStepPrint(trees)

                // big step
                bigStepPrint(trees)
            }
            case e => {
                println(e)
            }
        }
    }

    /**
      * Repeat the reduce step until it doesn't reduce anymore
      *
      * @param f The function to evaluate before each reduce
      */
    @tailrec def multiReduce(f: Term => Unit)(term: Term): Term = {
        f(term)
        val next = reduce(term)
        if (next == term) term else multiReduce(f)(next)
    }

    /*
   * Recursively reduces and prints intermediate result
   * Stops when reduction fixpoint reached and prints error if the result is not a terminal
   */
    def smallStepPrint(tree: Term): Unit = {
        val res = multiReduce(println)(tree)
        if (!res.isValue) println(s"Stuck term: $res")
    }

    def bigStepPrint(tree: Term): Unit = {
        val res = eval(tree)
        val stuck = if (!res.isValue) "Stuck term: " else ""
        println(s"Big step: ${stuck}$res")
    }
}
