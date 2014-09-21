package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract sealed class Term extends Positional {

    /** Returns true if this term is of numeric type */
    def isNumeric: Boolean = this match {
        case Zero => true
        case Pred(t) => t.isNumeric
        case Succ(t) => t.isNumeric
        case _ => false
    }

    /** Returns true if this term is a numeric literal */
    def isNumericLiteral: Boolean = this match {
        case Zero => true
        case Succ(t) => t.isNumericLiteral
        case _ => false
    }

    /** Returns true if this term is a value */
    def isValue: Boolean = this match {
        case True | False => true
        case t if t.isNumericLiteral => true
        case _ => false
    }
}

case object True extends Term
case object False extends Term
case object Zero extends Term

case class If(cond: Term, thn: Term, elz: Term) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term

case class StuckTerm(t: Term) extends Term
