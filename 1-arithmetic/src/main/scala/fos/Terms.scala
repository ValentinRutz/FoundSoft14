package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract sealed class Term extends Positional {

    /** Returns true if this term is a numeric value */
    def isNumericValue: Boolean = this match {
        case Zero => true
        case Pred(t) => t.isNumericValue
        case Succ(t) => t.isNumericValue
        case _ => false
    }

    /** Returns true if this term is a value */
    def isValue: Boolean = this match {
        case True | False => true
        case t if t.isNumericValue => true
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
