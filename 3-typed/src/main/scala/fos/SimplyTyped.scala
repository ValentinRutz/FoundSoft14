package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
  * This object implements a parser and evaluator for the
  *  simply typed lambda calculus found in Chapter 9 of
  *  the TAPL book.
  */
object SimplyTyped extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
        "pred", "iszero", "let", "in", "fst", "snd")

    /**
      * Term     ::= SimpleTerm { SimpleTerm }
      */
    def Term: Parser[Term] = positioned(
        rep1(SimpleTerm) ^^ {
            _ reduceLeft (Application(_, _))
        }
            | failure("illegal start of term"))

    /**
      * SimpleTerm ::= "true"
      *               | "false"
      *               | number
      *               | "succ" Term
      *               | "pred" Term
      *               | "iszero" Term
      *               | "if" Term "then" Term "else" Term
      *               | ident
      *               | "\" ident ":" Type "." Term
      *               | "(" Term ")"
      *               | "let" ident ":" Type "=" Term "in" Term
      *               | "{" Term "," Term "}"
      *               | "fst" Term
      *               | "snd" Term
      */
    def SimpleTerm: Parser[Term] = positioned(
        "true" ^^^ True
            | "false" ^^^ False
            | numericLit ^^ { case e => Iterator.iterate[Term](Zero)(Succ).toStream(e.toInt) }
            | "succ" ~> Term ^^ Succ
            | "pred" ~> Term ^^ Pred
            | "iszero" ~> Term ^^ IsZero
            | ("if" ~> Term) ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ { case c ~ t ~ e => If(c, t, e) }
            | ident ^^ {
                case ident => Variable(ident)
            }
            | ("\\" ~> ident) ~ (":" ~> Type) ~ ("." ~> Term) ^^ {
                case param ~ typ ~ body => Abstraction(Variable(param), typ, body)
            }
            | "(" ~> Term <~ ")"
            //   ... To complete ... with let and pair
            | ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ^^ {
                case param ~ typ ~ expr => Let(Variable(param), typ, expr)
            }
            | ("{" ~> Term <~ ",") ~ (Term <~ "}") ^^ {
                case fst ~ snd => Pair(fst, snd)
            }
            | "fst" ~> Term ^^ Fst
            | "snd" ~> Term ^^ Snd
            | failure("illegal start of simple term"))

    /**
      * Type       ::= SimpleType [ "->" Type ]
      */
    def Type: Parser[Type] = positioned(
        SimpleType ~ opt("->" ~> Type) ^^ {
            case from ~ Some(to) =>
                TypeFun(from, to)
            case typ ~ None => typ
        }
            | failure("illegal start of type"))

    def SimpleType: Parser[Type] = positioned(
        "Bool" ^^^ TypeBool
            | "Nat" ^^^ TypeNat
            | "(" ~> Type <~ ")"
            | failure("illegal start of type"))

    /** Thrown when no reduction rule applies to the given term. */
    case class NoRuleApplies(t: Term) extends Exception(t.toString)

    /** Print an error message, together with the position where it occured. */
    case class TypeError(pos: Position, msg: String) extends Exception(msg) {
        override def toString =
            msg + "\n" + pos.longString
    }

    /** The context is a list of variable names paired with their type. */
    type Context = List[(String, Type)]

    /** Is the given term a numeric value? */
    def isNumericVal(t: Term): Boolean = t match {
        case Zero => true
        case Succ(t) => isNumericVal(t)
        case _ => false
    }

    /** Is the given term a value? */
    def isValue(t: Term): Boolean = t match {
        case True | False | Abstraction(_, _, _) => true
        case nv if (isNumericVal(nv)) => true
        case _ => false
    }

    object freshName {
        var names: Set[String] = Set.empty
        def namesOf(term: Term): Set[String] = term match {
            case Variable(name) => Set(name)
            case Abstraction(param, typ, body) => namesOf(body) + param.name
            case Application(fun, arg) => namesOf(fun) ++ namesOf(arg)
            case Succ(term) => namesOf(term)
            case Pred(term) => namesOf(term)
            case IsZero(term) => namesOf(term)
            case If(c, t, e) => namesOf(c) ++ namesOf(t) ++ namesOf(e)
        }
        def addNames(term: Term): Unit = {
            names = names ++ namesOf(term)
        }
        val Versioned = """([^\$]+)\$(\d+)""".r
        var counter = 0
        def apply(name: String): String = {
            val realName = name match {
                case Versioned(n, v) => n
                case _ => name
            }
            counter = counter + 1
            val newName = (realName + "$" + counter)
            if (names contains newName) freshName(realName)
            else {
                names = names + newName
                newName
            }
        }

        def apply(variable: Variable): Variable = {
            val Variable(name) = variable
            Variable(freshName(name))
        }
    }

    /**
      * Free variables extraction
      *
      * @param t the term to compute
      * @return the set of free variables in t
      */
    // TODO: Add cases for Let, Pair, Fst, Snd
    def freeVars(t: Term): Set[Variable] = t match {
        case x: Variable => Set(x)
        case Abstraction(x, typ, t1) => freeVars(t1) - x
        case Application(t1, t2) => freeVars(t1) ++ freeVars(t2)
        case Succ(t) => freeVars(t)
        case Pred(t) => freeVars(t)
        case IsZero(t) => freeVars(t)
        case If(c, t, e) => freeVars(c) ++ freeVars(t) ++ freeVars(e)
    }

    /**
      * Alpha-conversion
      * Only renames abstraction since they are the only
      * way to bound variable names
      *
      */
    def alpha(tree: Abstraction): Abstraction = {
        val Abstraction(variable, typ, body) = tree
        val freshVar = freshName(variable)

        /**
          * Concretely makes substitution of alpha-conversion in given term.
          * This function may rename even variables shadowing the one that
          * has to be replaced, which does not change the behavior of the program
          * but makes useless conversion.
          * Note that this function avoids calling subst and prevents hidden infinite
          * recursion.
          *
          * @param tree The term to be renamed
          * @param oldVar The variable that will be substituted
          * @param freshVar The new variable for substitution
          * @return term with substitution applied
          */
        // TODO: Add cases for Let, Pair, Fst, Snd
        def renameTree(tree: Term)(implicit oldFresh: (Variable, Variable)): Term = {
            val (oldVar, freshVar) = oldFresh
            tree match {
                case v: Variable if (v == oldVar) =>
                    freshVar
                case v: Variable => v
                case Abstraction(param, typ, body) if (param == oldVar) =>
                    Abstraction(freshVar, typ, renameTree(body))
                case Abstraction(param, typ, body) =>
                    Abstraction(param, typ, renameTree(body))
                case Application(fun, arg) =>
                    Application(renameTree(fun), renameTree(arg))
                case Succ(term) =>
                    Succ(renameTree(term))
                case Pred(term) =>
                    Pred(renameTree(term))
                case IsZero(term) =>
                    IsZero(renameTree(term))
                case If(c, t, e) =>
                    If(renameTree(c), renameTree(t), renameTree(e))
            }
        }
        Abstraction(freshVar, typ, renameTree(body)((variable, freshVar)))
    }

    /**
      * Substitution method
      * substitute and rename variables if necessary to avoid free variable capture
      *
      * @param tree the tree to substitute
      * @param x the name of variable to substitute
      * @param s the term to substitute variable with
      * @return a new corresponding tree with substitution applied
      */
    // TODO: Add cases for Let, Pair, Fst, Snd
    def subst(tree: Term)(implicit x: String, s: Term): Term = tree match {
        case Variable(name) if (name == x) => s
        case Application(fun, arg) => Application(subst(fun), subst(arg))
        case a @ Abstraction(param, _, _) if (param.name == x) => a
        case Abstraction(param, typ, body) if (!freeVars(s).contains(param)) => {
            Abstraction(param, typ, subst(body))
        }
        case a @ Abstraction(_, _, _) => {
            // param.name != x, freeVars(s) contain param
            subst(alpha(a))
        }
        case Succ(term) => Succ(subst(term))
        case Pred(term) => Pred(subst(term))
        case IsZero(term) => IsZero(subst(term))
        case If(c, t, e) => If(subst(c), subst(t), subst(e))
        case _ => tree
    }

    // TODO : Confirm that Environment is not affected by alpha-conversion and substitutions !
    // TODO : Don't forget previous TODO

    /** Call by value reducer. */
    // Note: many points are simplified from untyped reducer, since bad input does not typechecks
    def reduce(t: Term): Term = t match {
        // COMPUTATION
        case If(True, t, _) => t
        case If(False, _, f) => f
        case IsZero(Zero) => True
        case IsZero(Succ(_)) =>
            False

            throw NoRuleApplies(t)
    }

    /**
      * Returns the type of the given term <code>t</code>.
      *
      *  @param ctx the initial context
      *  @param t   the given term
      *  @return    the computed type
      */
    def typeof(ctx: Context, t: Term): Type = t match {
        // Simple terms
        // Booleans
        case True | False =>
            TypeBool
        case IsZero(subterm) if typeof(ctx, subterm) == TypeNat =>
            TypeBool
        // Natural integers
        case Zero => TypeNat
        case Succ(subterm) if typeof(ctx, subterm) == TypeNat =>
            TypeNat
        case Pred(subterm) if typeof(ctx, subterm) == TypeNat =>
            TypeNat
        case If(cond, thenn, elz) if typeof(ctx, cond) == TypeBool &&
            typeof(ctx, thenn) == typeof(ctx, elz) =>
            typeof(ctx, thenn)
        case Variable(name) if ctx.exists(_._1 == name) =>
            (ctx find (_._1 == name)).get._2
        /*
        case If(c, t, e) if typeof(ctx, c) == TypeBool =>
            if (typeof(ctx, t) == typeof(ctx, e))
                typeof(ctx, e)
            else
                throw TypeError(t.pos, s"""\"Then\" and \"Else\" part of If"
                           expression do not share the same type""")
        case Abstraction(Variable(param), typParam, body) =>
            TypeFun(typParam, typeof((param, typParam) :: ctx, body))
        case v @ Variable(name) =>
            ctx.find(e: (String, Type) => e._1 == name).getOrElse(
                throw TypeError(v.pos, ""))._2
        case Let(_, typ, _) =>
            typ
        case Pair(fst, snd) =>
            TypePair(typeof(ctx, fst), typeof(ctx, snd))
        case Fst(Pair(fst, snd)) =>
            typeof(ctx, fst)
        case Snd(Pair(fst, snd)) =>
            typeof(ctx, snd)
            */
        case _ =>
            throw TypeError(t.pos, s"Illegally typed expression: $t")

    }

    /**
      * Returns a stream of terms, each being one step of reduction.
      *
      *  @param t      the initial term
      *  @param reduce the evaluation strategy used for reduction.
      *  @return       the stream of terms representing the big reduction.
      */
    def path(t: Term, reduce: Term => Term): Stream[Term] =
        try {
            var t1 = reduce(t)
            Stream.cons(t, path(t1, reduce))
        } catch {
            case NoRuleApplies(_) =>
                Stream.cons(t, Stream.empty)
        }

    def main(args: Array[String]): Unit = {
        val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
        phrase(Term)(tokens) match {
            case Success(trees, _) =>
                try {
                    println("typed: " + typeof(Nil, trees))
                    for (t <- path(trees, reduce))
                        println(t)
                } catch {
                    case tperror: TypeError => println(tperror.toString)
                }
            case e =>
                println(e)
        }
    }
}
