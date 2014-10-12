package fos

import scala.Console._
import Untyped._
import scala.annotation.tailrec

object Repl {

    type Definition = (String, Abstraction)
    type Environment = Map[String, Abstraction]

    val Empty: Environment = Map.empty[String, Abstraction]
    val UsefulDefs: Seq[String] = Seq(
        """let tru = \t. \f. t""",
        """let fls = \t. \f. f""",
        """let test = \l. \m. \n. l m n""",
        """let and = \b. \c. b c fls""",
        """let or = \c. \b. c tru b""",
        """let not = \c. c fls tru""",
        """let pair = \f. \s. \b. b f s""",
        """let fst = \p. p tru""",
        """let snd = \p. p fls""",
        """let c0 = \s. \z. z""",
        """let scc = \n. \s. \z. s (n s z)""")
    val PreFilled = UsefulDefs.foldLeft(Empty)((e, t) => eval(t, e))
    val Prompt: String = "lambda> "

    /** Maps from ParseResult types to Option */
    def parse(s: String): Option[Term] = {
        val tokens = new lexical.Scanner(s)
        phrase(Term)(tokens) match {
            case Success(tree, _) => Some(tree)
            case _ => None
        }
    }

    /**
      * We define definitions as the keywork let followed by an equal sign
      * followed by a string that parses to an abstraction
      */
    object Definition {
        val Def = """\s*let\s+(\w+)\s*=(.*)""".r
        def unapply(s: String): Option[(String, Term)] = s match {
            case Def(name, body) => parse(body) map (term => name -> term)
            case _ => None
        }
    }

    /** Matches an expression that parses succesfully to a Term */
    object Expression {
        def unapply(s: String): Option[Term] = parse(s)
    }

    /**
      * Used to match a string that contains "exit" possibly surrounded by
      * spaces
      */

    object Exit extends Throwable {
        def unapply(s: String): Option[Unit] =
            if (s.trim == "exit") Some() else None
    }

    /** Update the environment with the new definition */
    def update(env: Environment, let: Definition): Environment = env + let

    /** Rewrites a term with the abstractions defined in the environmnet */
    def rewrite[T <: Term](t: T)(implicit env: Environment): Term = t match {
        case Variable(x) => env.get(x).getOrElse(Variable(x))
        case Application(fun, arg) => Application(rewrite(fun), rewrite(arg))
        case Abstraction(name, body) => {
            /** Don't rewrite bound variables */
            val newEnv: Environment = env - name.name
            Abstraction(name, rewrite(body)(newEnv))
        }
    }

    /**
      *   Evaluate one line entered by the user and update the environment if
      *   needed
      */
    def eval(line: String, defs: Environment): Environment =
        line match {
            case Exit() => throw Exit
            case Definition(let) => {
                val (name, term) = let
                val rewritten = rewrite(term)(defs)

                if (!freeVars(rewritten).isEmpty) {
                    println("Definitions can't have free variables")
                    defs
                } else {
                    path(rewritten, reduceNormalOrder).last match {
                        case a: Abstraction =>
                            println(s"new definition : $name => $term")
                            update(defs, name -> a)
                        case _ =>
                            println("Illegal definition")
                            defs
                    }
                }
            }
            case Expression(expr) => {
                println("Evaluating expression ...")
                val reductions = path(rewrite(expr)(defs), reduceNormalOrder)
                reductions.zipWithIndex foreach {
                    case (tree, i) => println(s"$i. $tree")
                }
                val names = possibleNames(reductions.last, defs)
                println("Possible names for result : " + names.mkString(" "))
                defs
            }
            case _ => {
                println("Couldn't understand what you want ...")
                defs
            }

        }

    /** Checks that two expressions are structurally equivalent */
    def equals(t1: Term, t2: Term): Boolean = (t1, t2) match {
        case (Variable(v1), Variable(v2)) => v1 == v2
        case (Application(f1, a1), Application(f2, a2)) =>
            equals(f1, f2) && equals(a1, a2)
        case (a: Abstraction, Abstraction(p2, b2)) =>
            val Abstraction(p1, b1) = alpha(a)
            equals(b1, subst(b2)(p2.name, p1))
        case (_, _) => false
    }

    def possibleNames(t: Term, defs: Environment): Seq[String] = {
        defs.filter { case (name, body) => equals(t, body) }.map(_._1).toSeq
    }

    @tailrec def repl(input: => String, defs: Environment = PreFilled): Unit = {
        val newDefs = eval(input, defs)
        repl(input, newDefs)
    }

    /**
      * To launch use command "console" in sbt and then:
      *     :power
      *     fos.Repl.launch(repl.in.readLine _)
      */
    def launch(readLine: (String => String)): Unit = {
        try {
            repl(readLine(Prompt))
        } catch {
            case Exit => println("Good bye")
        }
    }

}
