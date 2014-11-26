package fos

import scala.collection.immutable.{ Set, ListSet }

abstract class Type {
    override def toString() = this match {
        case TypeVar(a) => a
        case TypeFun(a, b) => "(" + a + " -> " + b + ")"
        case TypeNat => "Nat"
        case TypeBool => "Bool"
    }
}

case class TypeVar(name: String) extends Type
case class TypeFun(argTyp: Type, bodyTyp: Type) extends Type
object TypeNat extends Type
object TypeBool extends Type
object TypeEmpty extends Type
//   ... To complete ... 

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
    /*
     * To be put in the environment, every Type has to be generalized to a
     * TypeScheme. This is done by putting as TypeVar (?) all Types That are not
     * bound by the environment
     * -> def generalize(env: Env, typ: Type): TypeScheme
     * generalize implemented in Type object
     */
    //   ... To complete ... 
    override def toString() = args.mkString("[", ", ", "].") + tp

    /*
     * 
     */
    def instantiate: Type = ???
}

object Type {
    //   ... To complete ... 
    type Env = List[(String, TypeScheme)]
    def generalize(env: Env, typ: Type): TypeScheme =
        TypeScheme(collectTypeVar(typ) filterNot (collectTypeVar(env).contains(_)),
            typ)

    def collectTypeVar(env: Env): List[TypeVar] = env flatMap { e =>
        collectTypeVar(e._2.tp)
    }

    def collectTypeVar(typ: Type): List[TypeVar] = typ match {
        case tv @ TypeVar(_) => List(tv)
        case TypeFun(t1, t2) => collectTypeVar(t1) ++ collectTypeVar(t2)
        case TypeNat | TypeBool => Nil
    }

    def freshTypeVar: Type = TypeVar(freshName)
    def freshName: String = {
        counter = counter + 1
        "x$" + counter
    }
    private var counter = 0
}

abstract class Substitution extends (Type => Type) { self =>

    def lookup(t: TypeVar): Option[Type]
    var indent = 0

    //   ... To complete ... 
    def apply(tp: Type): Type = {
        //println("  " * indent + "in: " + tp + "   subst: " + this)
        indent = indent + 1
        val result = tp match {
            case TypeBool | TypeNat => tp
            case TypeFun(from, to) => TypeFun(this(from), this(to))
            case tv @ TypeVar(_) => lookup(tv) getOrElse tv
        }
        indent = indent - 1
        //println("  " * indent + "out: " + result + "   subst: " + this)
        result
    }
    override def toString() = ""

    def apply(p: (Type, Type)): (Type, Type) = p match {
        case Pair(t1, t2) => (this(t1), this(t2))
    }

    def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
        env map { (pair) =>
            (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp)))
        }

    /*
       * priority to rhs of composition
       * first try to substitute with rhs, then lhs
       */
    def compose(that: Substitution): Substitution = new Substitution {
        def lookup(t: TypeVar) = that.lookup(t) match {
            case None => self.lookup(t)
            case some => some
        }
    }

    def composeWithPair(that: (Type, Type)): Substitution =
        compose(new SingletonSubst(that._1, that._2))

    def o(that: (Type, Type)): Substitution = composeWithPair(that)

    //   ... To complete ... 
}

class SingletonSubst(from: Type, to: Type) extends Substitution {
    def lookup(t: TypeVar) = if (t == from) Some(to) else None
}

/** The empty substitution. */
object emptySubst extends Substitution {
    def lookup(t: TypeVar) = None
}
