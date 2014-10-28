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

case class Abstraction(param: Variable, typ: Type, body: Term) extends Term {
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
case class Let(ident: Variable, typ: Type, expr: Term) extends Term {
    override def toString() = s"""let $ident: $typ = $expr"""
}

case class Pair(fst: Term, snd: Term) extends Term {
    override def toString() = s"""{ $fst, $snd }"""
}

case class Fst(pair: Term) extends Term {
    override def toString() = s"""fst $pair"""
}

case class Snd(pair: Term) extends Term {
    override def toString() = s"""snd $pair"""
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type {
    override def toString() = "Bool"
}

case object TypeNat extends Type {
    override def toString() = "Nat"
}

case class TypeFun(from: Type, to: Type) extends Type {
    override def toString() = from match {
        case _: TypeFun => s"($from)->$to"
        case _ => s"$from->$to"
    }
}

case class TypePair(fst: Type, snd: Type) extends Type {
    override def toString() = s"$fst*$snd"
}
