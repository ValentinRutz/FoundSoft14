package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 
case object True extends Term {
  override def toString() = "true"
}

case object False extends Term {
  override def toString() = "false"
}

case class If(cond: Term, zen: Term, elz: Term) extends Term {
  override def toString() = "if "+ cond +" then "+ zen +" else "+ elz
}

case object Zero extends Term {
  override def toString() = "0"
}

case class Succ(t: Term) extends Term {
  override def toString() = "succ "+ t
}

case class Pred(t: Term) extends Term {
  override def toString() = "pred "+ t
}

case class IsZero(t: Term) extends Term {
  override def toString() = "iszero "+ t
}
