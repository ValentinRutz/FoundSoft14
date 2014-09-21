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
        | numericLit ^^ {
            case e => {
                @tailrec def reduce(e: Int, acc: Term): Term = {
                    if (e == 0) return acc
                    reduce(e - 1, Succ(acc))
                }

                reduce(e.toInt, Zero)
            }
        }
        | failure("illegal start of expression"))

    def oneStepEvaluator(t: Term): Term =
        t match {
            case IsZero(Zero) => True
            case IsZero(Succ(nv)) if (isNumericValue(nv)) => False
            case IsZero(s) => IsZero(oneStepEvaluator(s))
            case Succ(s) => Succ(oneStepEvaluator(s))
            case Pred(Succ(nv)) if (isNumericValue(nv)) => nv
            case Pred(Zero) => Zero
            case Pred(s) => Pred(oneStepEvaluator(s))
            case If(True, t, e) => t
            case If(False, t, e) => e
            case If(c, t, e) => If(oneStepEvaluator(c), t, e)
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

    def main(args: Array[String]): Unit = {
        val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
        phrase(Expr)(tokens) match {
            case Success(trees, _) =>
                println(trees)
                println(oneStepEvaluator(trees))
            case e =>
                println(e)
        }
    }
}
