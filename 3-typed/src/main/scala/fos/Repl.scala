package fos

import scala.Console._
import SimplyTyped._
import scala.annotation.tailrec

object Repl {

    var num = 0
    def getFreshName = {
        num += 1
        s"res$num"
    }

    type Definition = (String, Term)
    type Environment = Seq[Definition]

    val Empty: Environment = Nil
    val UsefulDefs: Seq[String] = Seq(
        """def add=fix \a:Nat->Nat->Nat.(\x:Nat.(\y:Nat.if iszero x then y else a (pred x) (succ y)))""",
        """def mul=fix \m:Nat->Nat->Nat.(\x:Nat.(\y:Nat.if iszero x then 0 else add y (m (pred x) y)))""",
        """def fact=fix \f:Nat->Nat.(\d:Nat.if iszero d then succ 0 else mul d (f (pred d)))""")
    val Prompt: String = "lambda> "

    /** Maps from ParseResult types to Option */
    def parse(s: String): Option[Term] = {
        val tokens = new lexical.Scanner(s)
        phrase(Term)(tokens) match {
            case Success(tree, _) => Some(tree)
            case Failure(m, t) => println(m); println(t); None
        }
    }

    /**
      * We define definitions as the keywork def followed by an equal sign
      * followed by a string that parses to a value
      */
    object Definition {
        val Def = """\s*def\s+(\w+)\s*=(.*)""".r
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

    object Command {
        val Command = """:(.*)""".r
        def unapply(s: String): Option[Environment => Environment] = s match {
            case Command("help") => Some { e => println("This is the help"); e }
            case Command("env") => Some { e => println(e.mkString("\n")); e }
            case _ => None
        }
    }

    /** Update the environment with the new definition */
    def update(env: Environment, deff: Definition): Environment = deff +: env

    /** Rewrites a term with the abstractions defined in the environmnet */
    def rewrite[T <: Term](t: T)(implicit env: Environment): Term = env match {
        case Nil => t
        case (name, body) +: tail => rewrite(subst(t)(name, body))(tail)
    }

    /**
      * Evaluate one line entered by the user and update the environment if
      * needed
      *
      * @param line The line to evaluate
      * @param reducer The term evaluation function
      * @param defs The environment that contains all the named abstractions
      */
    def eval(line: String)(implicit reducer: Term => Term, defs: Environment): Environment =
        try {
            line match {
                case Exit() => throw Exit
                case Command(f) => f(defs)
                case Definition(deff) => {
                    val (name, term) = deff
                    val rewritten = rewrite(term)
                    val typ = typeof(rewritten)
                    path(rewritten, reducer).last match {
                        case Value(a) =>
                            println(s"$name : $typ = ${pretty(term)}")
                            update(defs, name -> a)
                        case _ =>
                            println("Illegal definition, not a value")
                            defs
                    }
                }
                case Expression(expr) => {
                    println("Evaluating expression ...")
                    val rewritten = rewrite(expr)
                    val typ = typeof(rewritten)
                    val reductions = path(rewritten, reducer)
                    reductions.zipWithIndex foreach {
                        case (tree, i) => println(s"$i. ${pretty(tree)}")
                    }
                    val name = getFreshName
                    val term = reductions.last
                    println(s"$name : $typ = ${pretty(term)}")
                    update(defs, name -> term)
                }
                case _ => {
                    println("Couldn't understand what you want ...")
                    defs
                }

            }
        } catch {
            case tperror: TypeError =>
                println(tperror)
                defs
        }

    @tailrec def repl(inputSrc: => String)(implicit reducer: Term => Term, defs: Environment = Empty): Unit = {
        val newDefs = try {
            /* Evaluate one line of user input */
            eval(inputSrc)(reducer, defs)
        } catch {
            case Exit => throw Exit
            case e: Throwable =>
                e.printStackTrace()
                defs
        }
        /** Repeat loop */
        repl(inputSrc)(reducer, newDefs)
    }

    def pretty(t: Term): String = t match {
        case NumericValue(t) => NumericValue.getValue(t).toString
        case Pred(t) => s"pred ${pretty(t)}"
        case Abstraction(v, t, b) => s"\\$v:$t.${pretty(b)}"
        case IsZero(t) => s"iszero ${pretty(t)}"
        case If(c, t, e) =>
            s"if ${pretty(c)} then ${pretty(t)} else ${pretty(e)}"
        case Application(t1, t2) => {
            val fun = t1 match {
                case Prefix(_) => s"(${pretty(t1)})"
                case _ => s"${pretty(t1)}"
            }
            val arg = t2 match {
                case _: Application => s"(${pretty(t2)})"
                case Prefix(_) => s"(${pretty(t2)})"
                case _ => s"${pretty(t2)}"
            }
            s"$fun $arg"
        }
        case _ => t.toString
    }

    /**
      * To launch use command "console" in sbt and then:
      *     :power
      *     fos.Repl.launch(repl.in.readLine _)
      */
    def launch(readLine: (String => String),
               reducer: Term => Term = reduce): Unit = {
        /** Ignore empty lines and exit on Ctrl+D */
        @tailrec def myReadLine: String = {
            val optLine = readLine(Prompt)
            if (optLine == null) throw Exit

            val line = optLine.trim
            if (line.isEmpty) myReadLine else line
        }

        try {
            val preFilled = UsefulDefs.foldLeft(Empty)((e, t) => eval(t)(reducer, e))
            repl(myReadLine)(reducer, preFilled)
        } catch {
            case Exit => println("Good bye")
        }
    }
}
