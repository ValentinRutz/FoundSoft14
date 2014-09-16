package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  
  /** Expr ::= 'true'
      	| 'false'
      	| 'if' Expr 'then' Expr 'else' Expr
      	| '0'
      	| 'succ' Expr
      	| 'pred' Expr
      	| 'iszero' Expr
   */
  
  def Expr: Parser[Term] = (
      "true" ^^^ True
      | "false" ^^^ False
      | "0" ^^^ Zero
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^
      	{ case "if" ~ c ~ "then" ~ t ~ "else" ~ e => If(c, t, e)}
      | "succ" ~> Expr ^^ { case e => Succ(e) }
      | "pred" ~> Expr ^^ { case e => Pred(e) }
      | "iszero" ~> Expr ^^ { case e => IsZero(e) }
      | failure("illegal start of expression"))

  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) => println(trees)
  //   ... To complete ... 
      case e =>
        println(e)
    }
  }
}
