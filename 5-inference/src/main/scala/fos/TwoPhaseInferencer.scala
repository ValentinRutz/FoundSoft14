package fos
import Type._

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
    import Type._

    type Constraint = (Type, Type)

    val noConstraints: List[Constraint] = Nil
    case class TypingResult(tpe: Type, c: List[Constraint])

    /**
      * Type <code>t</code> in <code>env</code> and return its type and a
      *  constraint list.
      */
    def collect(env: Env, t: Term): TypingResult = t match {
        case True() | False() => TypingResult(TypeBool, noConstraints)
        case Zero() => TypingResult(TypeNat, noConstraints)
        case Pred(t) => {
            val TypingResult(typ, constraints) = collect(env, t)
            TypingResult(TypeNat, (TypeNat, typ) :: constraints)
        }
        case Succ(t) => {
            val TypingResult(typ, constraints) = collect(env, t)
            TypingResult(TypeNat, (TypeNat, typ) :: constraints)
        }
        case IsZero(t) => {
            val TypingResult(typ, constraints) = collect(env, t)
            TypingResult(TypeBool, (TypeNat, typ) :: constraints)
        }
        case If(c, t, e) => {
            val TypingResult(cTyp, cCons) = collect(env, c)
            val TypingResult(tTyp, tCons) = collect(env, t)
            val TypingResult(eTyp, eCons) = collect(env, e)
            TypingResult(tTyp,
                (cTyp, TypeBool) :: (tTyp, eTyp) :: cCons ::: tCons ::: eCons)
        }
        case Var(x) => {
            val t1 = lookup(env, x)
            if (t1 == null)
                throw TypeError("Unknown variable " + x)
            TypingResult(t1.instantiate, noConstraints)
        }
        case Abs(v, tp, t) => {
            val tArg = tp match {
                case EmptyType => freshTypeVar
                case some => toType(some)
            }

            val tScheme = generalize(env, tArg)
            val TypingResult(tRet, constraints) = collect((v, tScheme) :: env, t)
            // Use tArg or tScheme.instantiate ?
            TypingResult(TypeFun(tArg, tRet), constraints)
        }
        case App(t1, t2) => {
            val TypingResult(t1Type, t1Cons) = collect(env, t1)
            val TypingResult(t2Type, t2Cons) = collect(env, t2)
            val resultType = freshTypeVar
            TypingResult(resultType,
                (t1Type, TypeFun(t2Type, resultType)) :: t1Cons ::: t2Cons)
        }
    }

    /**
      */
    def unify(c: List[Constraint]): Substitution =
        if (c.isEmpty) emptySubst
        else c.head match {
            case (TypeVar(a), TypeVar(b)) if (a == b) =>
                unify(c.tail)
            //   ... To complete ... 
            case (t1, t2) =>
                throw TypeError("Could not unify: " + t1 + " with " + t2)
        }

    override def typeOf(t: Term): Type = try {
        val TypingResult(tp, c) = collect(Nil: Env, t)
        val s = unify(c)
        s(tp)
    } catch {
        case TypeError(msg) =>
            Console.println("type error: " + msg)
            null
    }

}
