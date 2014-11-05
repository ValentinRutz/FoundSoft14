package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
  * This object implements a parser and evaluator for the
  *  simply typed lambda calculus found in Chapter 9 of
  *  the TAPL book.
  */
object SimplyTyped extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}",
        ",", "*", "=>", "|", "+")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
        "pred", "iszero", "let", "in", "fst", "snd", "inl", "inr", "as", "case",
        "of", "fix", "letrec")

    /**
      * Term     ::= SimpleTerm { SimpleTerm }
      */
    def Term: Parser[Term] = positioned(
        rep1(SimpleTerm) ^^ {
            _ reduceLeft (Application(_, _))
        }
            | failure("illegal start of term"))

    /**
      * SimpleTerm ::=
      *       "true"
      *     | "false"
      *     | number
      *     | "succ" Term
      *     | "pred" Term
      *     | "iszero" Term
      *     | "if" Term "then" Term "else" Term
      *     | ident
      *     | "\" ident ":" Type "." Term
      *     | "{" Term "," Term "}"
      *     | "fst" Term
      *     | "snd" Term
      *     | "inl" Term "as" Type
      *     | "inr" Term "as" Type
      *     | "case" Term "of" "inl" ident "=>" Term "|" "inr" ident "=>" Term
      *     | "fix" Term
      */
    def SimpleTerm: Parser[Term] = positioned(
        "true" ^^^ True()
            | "false" ^^^ False()
            | numericLit ^^ {
                case e => (1 to e.toInt).foldLeft[Term](Zero())((b, a) => Succ(b))
            }
            | "succ" ~> Term ^^ Succ
            | "pred" ~> Term ^^ Pred
            | "iszero" ~> Term ^^ IsZero
            | ("if" ~> Term) ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ {
                case cond ~ thn ~ els => If(cond, thn, els)
            }
            | ident ^^ Variable
            | ("\\" ~> ident) ~ (":" ~> Type) ~ ("." ~> Term) ^^ {
                case param ~ typ ~ body => Abstraction(Variable(param), typ, body)
            }
            | "(" ~> Term <~ ")"
            | ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term) ^^ {
                case param ~ typ ~ expr ~ term =>
                    Application(Abstraction(Variable(param), typ, term), expr)
            }
            | ("{" ~> Term <~ ",") ~ (Term <~ "}") ^^ {
                case fst ~ snd => Pair(fst, snd)
            }
            | "fst" ~> Term ^^ Fst
            | "snd" ~> Term ^^ Snd
            | ("inl" ~> Term) ~ ("as" ~> Type) ^^ {
                case term ~ typ => InjectLeft(term, typ)
            }
            | ("inr" ~> Term) ~ ("as" ~> Type) ^^ {
                case term ~ typ => InjectRight(term, typ)
            }
            | ("case" ~> Term) ~ ("of" ~> "inl" ~> ident) ~ ("=>" ~> Term) ~
            ("|" ~> "inr" ~> ident) ~ ("=>" ~> Term) ^^ {
                case elem ~ lVar ~ lTerm ~ rVar ~ rTerm =>
                    Case(elem, Variable(lVar), lTerm, Variable(rVar), rTerm)
            }
            | "fix" ~> Term ^^ Fix
            | ("letrec" ~> ident) ~ (":" ~> Type) ~
            ("=" ~> Term) ~ ("in" ~> Term) ^^ {
                case x ~ typ ~ t1 ~ t2 =>
                    val param = Variable(x)
                    val expr = Fix(Abstraction(param, typ, t1))
                    Application(Abstraction(param, typ, t2), expr)
            }
            | failure("illegal start of simple term"))

    /**
      * Type       ::= TupleType [ "->" Type ]
      */
    def Type: Parser[Type] = positioned(
        TupleType ~ opt("->" ~> Type) ^^ {
            case from ~ Some(to) =>
                TypeFun(from, to)
            case typ ~ None => typ
        }
            | failure("illegal start of type"))

    /**
      * TupleType       ::= SimpleType [ ("*" | "+") TupleType ]
      */
    def TupleType: Parser[Type] = positioned(
        SimpleType ~ opt(("*" | "+") ~ TupleType) ^^ {
            case fst ~ Some("*" ~ snd) => TypePair(fst, snd)
            case left ~ Some("+" ~ right) => TypeSum(left, right)
            case typ ~ None => typ
        }
            | failure("illegal start of type"))

    def SimpleType: Parser[Type] = positioned(
        "Bool" ^^^ TypeBool()
            | "Nat" ^^^ TypeNat()
            | "(" ~> Type <~ ")"
            | failure("illegal start of type"))

    /** Thrown when no reduction rule applies to the given term. */
    case class NoRuleApplies(t: Term) extends Exception(t.toString)

    /** Print an error message, together with the position where it occured. */
    case class TypeError(pos: Position, msg: String) extends Exception(msg) {
        override def toString =
            msg + "\n" + pos.longString
    }

    class ErrorParamType(pos: Position, expected: Type, found: Type)
        extends TypeError(pos,
            s"parameter type mismatch: expected $expected, found $found")

    /** The context is a list of variable names paired with their type. */
    type Context = List[(String, Type)]

    /**
      * Substitution method
      * substitute and rename variables if necessary to avoid free variable capture
      *
      * @param tree the tree to substitute
      * @param x the name of variable to substitute
      * @param s the term to substitute variable with
      * @return a new corresponding tree with substitution applied
      */
    def subst(tree: Term)(implicit x: String, s: Term): Term = tree match {
        case Variable(name) if (name == x) => s
        case Application(fun, arg) => Application(subst(fun), subst(arg))
        case Abstraction(param, typ, body) =>
            Abstraction(param, typ, substBindingTerm(param, body))
        case Succ(term) => Succ(subst(term))
        case Pred(term) => Pred(subst(term))
        case IsZero(term) => IsZero(subst(term))
        case If(c, t, e) => If(subst(c), subst(t), subst(e))
        case Pair(fst, snd) => Pair(subst(fst), subst(snd))
        case Fst(pair) => Fst(subst(pair))
        case Snd(pair) => Snd(subst(pair))
        case InjectLeft(elem, typ) => InjectLeft(subst(elem), typ)
        case InjectRight(elem, typ) => InjectRight(subst(elem), typ)
        case Case(elem, lVar, lTerm, rVar, rTerm) =>
            Case(subst(elem),
                lVar, substBindingTerm(lVar, lTerm),
                rVar, substBindingTerm(rVar, rTerm))
        case _ => tree
    }

    /**
      * Helper function that controls substitution in expression with bound
      * variable.
      * The concrete cases are Abstraction and Case branches
      */
    def substBindingTerm(binding: Variable, body: Term)(implicit x: String, s: Term): Term =
        if (binding.name == x) body else subst(body)

    /** Call by value reducer. */
    // Note: many points are simplified from untyped reducer, since bad input does not typecheck
    def reduce(t: Term): Term = t match {
        // COMPUTATION
        case If(True(), t, _) => t
        case If(False(), _, f) => f
        case IsZero(Zero()) => True()
        case IsZero(Succ(NumericValue(_))) => False()
        case Pred(Zero()) => Zero()
        case Pred(Succ(NumericValue(v))) => v
        case Application(Abstraction(param, typ, body), Value(value)) =>
            subst(body)(param.name, value)
        case Fst(Pair(Value(fst), Value(_))) => fst
        case Snd(Pair(Value(_), Value(snd))) => snd
        case Case(InjectLeft(Value(v), _), lVar, lTerm, _, _) =>
            subst(lTerm)(lVar.name, v)
        case Case(InjectRight(Value(v), _), _, _, rVar, rTerm) =>
            subst(rTerm)(rVar.name, v)
        // CONGRUENCE
        case If(c, t, e) => If(reduce(c), t, e)
        case IsZero(t) => IsZero(reduce(t))
        case Pred(t) => Pred(reduce(t))
        case Succ(t) => Succ(reduce(t))
        case Application(Value(v), t) => Application(v, reduce(t))
        case Application(t1, t2) =>
            Application(reduce(t1), t2)
        case Pair(Value(fst), snd) => Pair(fst, reduce(snd))
        case Pair(fst, snd) => Pair(reduce(fst), snd)
        case Fst(pair) => Fst(reduce(pair))
        case Snd(pair) => Snd(reduce(pair))
        case Case(elem, lVar, lTerm, rVar, rTerm) =>
            Case(reduce(elem), lVar, lTerm, rVar, rTerm)
        case InjectLeft(elem, typ) => InjectLeft(reduce(elem), typ)
        case InjectRight(elem, typ) => InjectRight(reduce(elem), typ)
        case _ => throw NoRuleApplies(t)
    }

    /**
      * Small helper function that checks that <code>t</code> is well typed with
      * type <code>expected</code> and returns <code>finalType</code> if it is
      * the case
      *
      * @param error The function that will generate the error message from the
      * position of the term, the expected type and the type that was found
      *
      */
    def expect(t: Term, expected: Type, finalType: Type,
               error: (Position, Type, Type) => TypeError)(implicit ctx: Context) = {
        val found = typeof(t)
        if (found == expected) finalType else throw error(t.pos, expected, found)
    }

    /**
      * Returns the type of the given term <code>t</code>.
      *
      *  @param ctx the initial context
      *  @param t   the given term
      *  @return    the computed type
      */
    def typeof(t: Term)(implicit ctx: Context = Nil): Type = t match {
        /* T-TRUE, T-FALSE */
        case True() | False() =>
            TypeBool()

        /* T-ZERO */
        case Zero() =>
            TypeNat()

        /* T-PRED */
        case Pred(subterm) =>
            expect(subterm, TypeNat(), TypeNat(), new ErrorParamType(_, _, _))

        /* T-SUCC */
        case Succ(subterm) =>
            expect(subterm, TypeNat(), TypeNat(), new ErrorParamType(_, _, _))

        /* T- ISZERO */
        case IsZero(subterm) =>
            expect(subterm, TypeNat(), TypeBool(), new ErrorParamType(_, _, _))

        /* T-IF */
        case If(cond, thenn, els) =>
            expect(cond, TypeBool(), TypeBool(),
                (pos, expected, found) =>
                    TypeError(pos, s"Condition should be a boolean. " +
                        "Found $found"))

            val typeThen = typeof(thenn)
            expect(els, typeThen, typeThen,
                (pos, expected, found) =>
                    TypeError(pos, s"then and else part should have the same type. " +
                        "Found: then=$expected != else=$found"))

        /* T-VAR */
        case v @ Variable(name) => (ctx find (_._1 == name)) map {
            case (name, typ) => typ
        } getOrElse {
            throw TypeError(v.pos, s"variable $name is not in the context")
        }

        /* T-ABS */
        case Abstraction(Variable(param), typ, body) =>
            implicit val newContext = (param, typ) :: ctx
            TypeFun(typ, typeof(body)(newContext))

        /* T-APP */
        case Application(t1, t2) =>
            val TypeFun(from, to) = typeof(t1) match {
                case typeT1: TypeFun => typeT1
                case _ =>
                    throw TypeError(t1.pos, s"left term should be an abstraction")
            }

            expect(t2, from, to, new ErrorParamType(_, _, _))

        case Pair(fst, snd) =>
            TypePair(typeof(fst), typeof(snd))

        case Fst(pair) => typeof(pair) match {
            case TypePair(fst, snd) => fst
            case err =>
                throw TypeError(pair.pos, s"pair type expected but $err found")
        }

        case Snd(pair) => typeof(pair) match {
            case TypePair(fst, snd) => snd
            case err =>
                throw TypeError(pair.pos, s"pair type expected but $err found")
        }

        /* T-INL */
        case InjectLeft(elem, typ @ TypeSum(left, _)) =>
            expect(elem, left, typ, new ErrorParamType(_, _, _))

        /* T-INR */
        case InjectRight(elem, typ @ TypeSum(_, right)) =>
            expect(elem, right, typ, new ErrorParamType(_, _, _))

        /* T-CASE */
        case Case(elem, lVal, lTerm, rVal, rTerm) => typeof(elem) match {
            case TypeSum(typeL, typeR) => {
                val finalTypeL = typeof(lTerm)((lVal.name, typeL) :: ctx)
                val finalTypeR = typeof(rTerm)((lVal.name, typeR) :: ctx)
                if (finalTypeL == finalTypeR) finalTypeL
                else throw TypeError(rTerm.pos, s"type mismatch : $finalTypeL expected but $finalTypeR found")
            }
            case err =>
                throw TypeError(elem.pos, s"sum type expected but $err found")
        }

        case x => throw new AssertionError(s"No rule to type term $x")

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
                    println("typed: " + typeof(trees))
                    for (t <- path(trees, reduce))
                        println(t)
                } catch {
                    case tperror: TypeError =>
                        println(tperror)
                }
            case e =>
                println(e)
        }
    }
}
