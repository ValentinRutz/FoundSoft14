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
//   ... To complete ... 

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
    /*
     * To be put in the environment, every Type has to be generalized to a
     * TypeScheme. This is done by putting as TypeVar (?) all Types That are not
     * bound by the environment
     * -> def generalize(env: Env, typ: Type): TypeScheme
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

abstract class Substitution extends (Type => Type) {

    var indent = 0

    //   ... To complete ... 
    def apply(tp: Type): Type = {
        //println("  " * indent + "in: " + tp + "   subst: " + this)
        indent = indent + 1
        val result = tp match {
            case _ => ??? //   ... To complete ... 
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
        env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

    //   ... To complete ... 
}

/** The empty substitution. */
object emptySubst extends Substitution {
    def lookup(t: TypeVar) = t
}
