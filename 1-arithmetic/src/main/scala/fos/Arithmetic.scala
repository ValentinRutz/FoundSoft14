package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.annotation.tailrec

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  /** Expr ::= 'true'
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
      | ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^
      	{ case c ~ t ~ e => If(c, t, e) }
      | "succ" ~> Expr ^^ Succ.apply _
      | "pred" ~> Expr ^^ Pred.apply _
      | "iszero" ~> Expr ^^ IsZero.apply _
      | numericLit ^^ { case e => {
    	  @tailrec
    	  def reduce(e: Int, acc: Term): Term = {
    	    if(e == 0) return acc
    	    reduce(e - 1, Succ(acc))
    	  }
    	  
    	  reduce(e.toInt, Zero)
      	}
      }
      | failure("illegal start of expression"))

  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) => println(trees) 
      case e =>
        println(e)
    }
  }
}
