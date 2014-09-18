package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

object True extends Term {
    override def toString = this.getClass.getSimpleName
}
object False extends Term {
    override def toString = this.getClass.getSimpleName
}
object Zero extends Term {
    override def toString = this.getClass.getSimpleName
}
case class If(cond: Term, thn: Term, elz: Term) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term
