package fos

import scala.util.parsing.input.Positional
import scala.annotation.tailrec

/** Abstract Syntax Trees for terms. */
sealed abstract class Term extends Positional

case class True() extends Term {
    override def toString = "true"
}

case class False() extends Term {
    override def toString = "false"
}

case class Zero() extends Term {
    override def toString = "0"
}

case class Succ(term: Term) extends Term {
    override def toString = s"succ $term"
}

case class Pred(term: Term) extends Term {
    override def toString = s"pred $term"
}

case class IsZero(term: Term) extends Term {
    override def toString = s"iszero $term"
}

case class If(cond: Term, thn: Term, els: Term) extends Term {
    override def toString =
        s"if $cond then $thn else $els"
}

case class Variable(name: String) extends Term {
    override def toString = name
}

case class Abstraction(param: Variable, typ: Type, body: Term) extends Term {
    override def toString = body match {
        case _: Abstraction => s"\\$param:$typ.($body)"
        case _ => s"\\$param:$typ.$body"
    }
}

case class Application(fun: Term, arg: Term) extends Term {

    private val parFun: String = fun match {
        case Prefix(_) => s"($fun)"
        case _ => s"$fun"
    }
    private val parArg: String = arg match {
        case _: Application => s"($arg)"
        case Prefix(_) => s"($arg)"
        case _ => s"$arg"
    }
    override def toString = s"$parFun $parArg"
}

case class Pair(fst: Term, snd: Term) extends Term {
    override def toString = s"""{$fst, $snd}"""
}

case class Fst(pair: Term) extends Term {
    override def toString = s"""fst $pair"""
}

case class Snd(pair: Term) extends Term {
    override def toString = s"""snd $pair"""
}

case class InjectLeft(elem: Term, typ: Type) extends Term {
    override def toString = s"inl $elem as $typ"
}

case class InjectRight(elem: Term, typ: Type) extends Term {
    override def toString = s"inr $elem as $typ"
}

case class Case(elem: Term,
                lVar: Variable, lTerm: Term,
                rVar: Variable, rTerm: Term) extends Term {
    override def toString =
        s"case $elem of inl $lVar => $lTerm | inr $rVar => $rTerm"
}
/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case class TypeBool() extends Type {
    override def toString = "Bool"
}

case class TypeNat() extends Type {
    override def toString = "Nat"
}

case class TypeFun(from: Type, to: Type) extends Type {
    override def toString = from match {
        case _: TypeFun => s"($from)->$to"
        case _ => s"$from->$to"
    }
}

case class TypePair(fst: Type, snd: Type) extends Type {
    override def toString = (fst, snd) match {
        case (TypeFun(_, _) | TypePair(_, _), TypeFun(_, _) | TypePair(_, _)) =>
            s"($fst)*($snd)"
        case (_, TypeFun(_, _) | TypePair(_, _)) => s"$fst*($snd)"
        case (TypeFun(_, _) | TypePair(_, _), _) => s"($fst)*$snd"
        case _ => s"$fst*$snd"
    }
}

case class TypeSum(left: Type, right: Type) extends Type {
    override def toString = s"$left + $right"
}

/* Useful extractors */

object Value {
    def unapply(t: Term): Option[Term] = t match {
        case True() | False() | Abstraction(_, _, _) => Some(t)
        case Pair(Value(_), Value(_)) => Some(t)
        case NumericValue(x) => Some(t)
        case _ => None
    }
}

object NumericValue {

    @tailrec private def isNumericValue(t: Term): Boolean = t match {
        case Zero() => true
        case Succ(x) => isNumericValue(x)
        case _ => false
    }

    def unapply(t: Term): Option[Term] = if (isNumericValue(t)) Some(t) else None
}

object Prefix {
    def unapply(t: Term): Option[Term] = t match {
        case IsZero(_)
            | Succ(_)
            | Pred(_)
            | Fst(_)
            | Snd(_)
            | If(_, _, _)
            | Abstraction(_, _, _) => Some(t)
        case _ => None
    }
}
