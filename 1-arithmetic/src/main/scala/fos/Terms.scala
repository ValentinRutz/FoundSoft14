package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

abstract class Terminal extends Term

case object True extends Terminal
case object False extends Terminal
case object Zero extends Terminal

case class If(cond: Term, thn: Term, elz: Term) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term

case class StuckTerm(t: Term) extends Term
