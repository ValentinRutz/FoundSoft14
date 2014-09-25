package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.annotation.tailrec
import java.lang.IllegalArgumentException
import Pipeline._

/**
  * This object implements a parser and evaluator for the NB
  *  language of booleans and numbers found in Chapter 3 of
  *  the TAPL book.
  */
object Arithmetic extends StandardTokenParsers {
    lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

    import lexical.NumericLit

    /** Simple exception for terms that cannot be evaluated */
    case class StuckTermException(tree: Term) extends Throwable

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
            case _ => throw new StuckTermException(tree)
        }

        /* B-SUCC */
        case Succ(t) => eval(t) match {
            case nv if nv.isNumericValue => Succ(nv)
            case _ => throw new StuckTermException(tree)
        }

        /* B-PREDZERO, B-PREDSUCC */
        case Pred(t) => eval(t) match {
            case Zero => Zero
            case Succ(nv) if nv.isNumericValue => nv
            case _ => throw new StuckTermException(tree)
        }

        /* B-ISZEROZERO, B-ISZEROSUCC */
        case IsZero(t) => eval(t) match {
            case Zero => True
            case Succ(nv) if nv.isNumericValue => False
            case _ => throw new StuckTermException(tree)
        }

        /* Error */
        case error => throw new IllegalArgumentException("Unexpected expression: " + error)
    }

    def main(args: Array[String]): Unit = {
        val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
        phrase(Expr)(tokens) match {
            case Success(trees, _) => {
                println(trees)

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

    /** Basic pipeline for printing the term going through */
    def printTerm: Pipeline[Term, Term] = (term: Term) => {
        println(term)
        term
    }

    /*
   * Recursively reduces and prints intermediate result
   * Stops when reduction fixpoint reached and prints error if the result is not a terminal
   */
    def smallStepPrint(tree: Term): Unit = {
        val stepPipe = printTerm | Pipeline(reduce)
        val pipe = repeatWhile(stepPipe, (a: Term, b: Term) => a != b)

        val res = pipe.run(tree)
        if (!res.isValue) println(s"Stuck term: $res")
    }

    def bigStepPrint(tree: Term): Unit = {
        try {
            val res = Pipeline(eval).run(tree)
            println(s"Big step: $res")
        } catch {
            case StuckTermException(term) => println(s"Big step: Stuck term: $term")
        }
    }
}
