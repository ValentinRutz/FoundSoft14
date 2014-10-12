package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
    def isValue = this match {
        case _: Abstraction => true
        case _: Variable | _: Application => false
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
    override def toString: String = {
        def subterm(t: Term): String = t match {
            case _: Application => s"($t)"
            case _ => t.toString
        }
        s"${subterm(function)} ${subterm(argument)}"
    }
}
