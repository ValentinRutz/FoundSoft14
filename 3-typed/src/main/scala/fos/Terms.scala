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
    override def toString = s"{$fst, $snd}"
}

case class Fst(pair: Term) extends Term {
    override def toString = s"fst $pair"
}

case class Snd(pair: Term) extends Term {
    override def toString = s"snd $pair"
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

case class Fix(func: Term) extends Term {
    override def toString = s"fix $func"
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
    private def fstStr = fst match {
        case _: TypeFun | _: TypePair | _: TypeSum => s"($fst)"
        case _ => s"$fst"
    }

    private def sndStr = snd match {
        case _: TypeFun => s"($snd)"
        case _ => s"$snd"
    }

    override def toString = s"$fstStr*$sndStr"
}

case class TypeSum(left: Type, right: Type) extends Type {
    private def lStr = left match {
        case _: TypeFun | _: TypePair | _: TypeSum => s"($left)"
        case _ => s"$left"
    }

    private def rStr = right match {
        case _: TypeFun => s"($right)"
        case _ => s"$right"
    }

    override def toString = s"$lStr+$rStr"
}

/* Useful extractors */

object Value {
    def unapply(t: Term): Option[Term] = t match {
        case True() | False() | Abstraction(_, _, _) => Some(t)
        case Pair(Value(_), Value(_)) => Some(t)
        case InjectLeft(Value(_), _) => Some(t)
        case InjectRight(Value(_), _) => Some(t)
        case NumericValue(_) => Some(t)
        case _ => None
    }
}

object NumericValue {

    @tailrec private def isNumericValue(t: Term): Boolean = t match {
        case Zero() => true
        case Succ(x) => isNumericValue(x)
        case _ => false
    }

    def getValue(t: Term) = {
        assume(isNumericValue(t))

        @tailrec def getValueRec(t: Term, accu: Int = 0): Int = t match {
            case Zero() => accu
            case Succ(pred) => getValueRec(pred, accu + 1)
        }

        getValueRec(t)
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
            | Abstraction(_, _, _)
            | Case(_, _, _, _, _)
            | Fix(_) => Some(t)
        case _ => None
    }
}
