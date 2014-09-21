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
        | "0" ^^^ Zero
        | ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case c ~ t ~ e => If(c, t, e) }
        | "succ" ~> Expr ^^ Succ
        | "pred" ~> Expr ^^ Pred
        | "iszero" ~> Expr ^^ IsZero
        | numericLit ^^ { case e => Iterator.iterate[Term](Zero)(Succ).drop(e.toInt).next }
        | failure("illegal start of expression"))

    /**
      * "one step" evaluator
      *
      * @param t The term to reduce
      * @return The result of one step of the evaluation
      */
    def reduce(t: Term): Term =
        t match {
            case IsZero(Zero) => True
            case IsZero(Succ(nv)) if (isNumericValue(nv)) => False
            case IsZero(s) => IsZero(reduce(s))
            case Succ(s) => Succ(reduce(s))
            case Pred(Succ(nv)) if (isNumericValue(nv)) => nv
            case Pred(Zero) => Zero
            case Pred(s) => Pred(reduce(s))
            case If(True, t, e) => t
            case If(False, t, e) => e
            case If(c, t, e) => If(reduce(c), t, e)
            case s @ (True | False | Zero) => s
            case error => throw new IllegalArgumentException("Unexpected expression: " + error)
        }

    def isNumericValue(tree: Term): Boolean =
        tree match {
            case Zero => true
            case Pred(t) => isNumericValue(t)
            case Succ(t) => isNumericValue(t)
            case _ => false
        }

    /**
      * "big step" evaluator
      *
      * @param tree the term to evaluate
      * @return  the result of the evaluator
      */
    def eval(tree: Term): Term = {
        def error(stuckTerm: Term, tree: Term) = stuckTerm match {
            case st: StuckTerm => st
            case st => StuckTerm(tree)
        }
        tree match {
            // B-VALUE
            case v: Terminal => v
            // B-IFTRUE, B-IFFALSE
            case If(c, t, e) => eval(c) match {
                case True => eval(t)
                case False => eval(e)
                case stuckTerm => error(stuckTerm, tree)
            }
            // B-SUCC
            case Succ(t) => eval(t) match {
                case nv if (isNumericValue(nv)) => nv
                case stuckTerm => error(stuckTerm, tree)
            }
            // B-PREDZERO, B-PREDSUCC
            case Pred(t) => eval(t) match {
                case Zero => Zero
                case Succ(nv) if (isNumericValue(nv)) => nv
                case stuckTerm => error(stuckTerm, tree)
            }
            // B-ISZEROZERO, B-ISZEROSUCC
            case IsZero(t) => eval(t) match {
                case Zero => True
                case Succ(nv) if (isNumericValue(nv)) => False
                case stuckTerm => error(stuckTerm, tree)
            }
            // never reached
            case stuckTerm => stuckTerm
        }
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

        /*
         * Recursively reduces and prints intermediate result
         * Stops when reduction fixpoint reached and prints error if the result is not a terminal
         */
        def smallStepPrint(tree: Term): Unit = {
            val reduced = reduce(tree)
            if (reduced == tree) tree match {
                case t: Terminal => // already printed in previous call
                case stuckTerm => println("Stuck term : " + stuckTerm)
            } else {
                println(reduced)
                smallStepPrint(reduced)
            }
        }

        def bigStepPrint(tree: Term): Unit = {
            print("Big step : ")
            eval(tree) match {
                case StuckTerm(stuckTerm) => println("Stuck term : " + stuckTerm)
                case term => println(term)
            }
        }
    }
}
