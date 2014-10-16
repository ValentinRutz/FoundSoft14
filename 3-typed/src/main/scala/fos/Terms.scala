package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
    override def toString() = "true"
}

case object False extends Term {
    override def toString() = "false"
}

case object Zero extends Term {
    override def toString() = "0"
}

case class Succ(term: Term) extends Term {
    override def toString() = term match {
        case Application(_, _) => s"succ ($term)"
        case _ => s"succ $term"
    }
}

case class Pred(term: Term) extends Term {
    override def toString() = term match {
        case Application(_, _) => s"pred ($term)"
        case _ => s"pred $term"
    }
}

case class IsZero(term: Term) extends Term {
    override def toString() = term match {
        case Application(_, _) => s"isZero ($term)"
        case _ => s"isZero $term"
    }
}

case class If(c: Term, t: Term, e: Term) extends Term {
    override def toString() =
        s"if $c then $t else $e"
}

case class Variable(name: String) extends Term {
    override def toString() = name
}

case class Abstraction(param: Variable, typ: Type, body: Type) extends Term {
    override def toString() =
        s"""\$param: $typ.body"""
}

case class Application(fun: Term, arg: Term) extends Term {
    private val parFun: String = fun match {
        case _: Abstraction | _: If => s"($fun)"
        case _ => s"$fun"
    }
    private val parArg: String = arg match {
        case _: Application => s"($arg)"
        case _ => s"$arg"
    }
    override def toString() = parFun + " " + parArg
}

// TODO complete to implement 'let' and 'pair'
/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type {
    override def toString() = "Bool"
}
//   ... To complete ... 
