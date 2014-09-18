package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

abstract class Terminal extends Term
object True extends Terminal {
    override def toString = "True"
}
object False extends Terminal {
    override def toString = "False"
}
object Zero extends Terminal {
    override def toString = "Zero"
}
case class If(cond: Term, thn: Term, elz: Term) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term
