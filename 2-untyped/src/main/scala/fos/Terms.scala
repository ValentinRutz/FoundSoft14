package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
    def isValue = this match {
        case Variable(_) | Abstraction(_, _) => true
        case Application(_, _) => false
    }
}

case class Variable(name: String) extends Term {
    override def toString: String = name
}

// TODO consider replacing param Variable by a simple String (variable's name
case class Abstraction(param: Variable, body: Term) extends Term {
    override def toString: String = "(\\" + param.toString + "." + body.toString + ")"
}

case class Application(function: Term, argument: Term) extends Term {
    override def toString: String = this match {
        case Application(f, a) if f.isValue && a.isValue => f.toString + " " + a.toString
        case Application(f, a) if f.isValue => f.toString + " (" + a.toString + ")"
        case Application(f, a) if a.isValue => "(" + f.toString + ") " + a.toString
        case Application(f, a) => "(" + f.toString + ") (" + a.toString + ")"
    }
}
