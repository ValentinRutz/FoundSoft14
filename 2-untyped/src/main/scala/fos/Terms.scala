package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 

case class Variable(name: String) extends Term {
  override def toString: String = name
}

case class Abstraction(param: Variable, body: Term) extends Term {
  override def toString: String = "\\" + Variable.toString + ". (" + body.toString + ")"
}

case class Application(function: Term, argument: Term) extends Term {
  override def toString: String = "(" + function.toString + ") (" + argument.toString + ")"
}
