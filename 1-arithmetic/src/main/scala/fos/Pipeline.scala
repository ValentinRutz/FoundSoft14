package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import Arithmetic._
import scala.util.parsing.input._
import scala.annotation.tailrec

/**
  * Created by Valentin on 18/09/14.
  */
abstract class Pipeline[-F, +T] {
    self =>

    def run(v: F): T

    def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
        def run(v: F): G = {
            val first = self.run(v)
            thenn.run(first)
        }
    }

    def |[G](thenn: Pipeline[T, G]): Pipeline[F, G] = andThen(thenn)
}

/** Exeption to return something meaningful in case of a parser pipeline that fails */
case class PipelineError(message: String) extends Exception

object Pipeline {

    /**
      * Implicit conversion from parser to pipeline
      * @param parse The parser being converted
      * @tparam T The type of the parser
      * @return A pipeline that throws runtime exception in case the parser fails
      */
    implicit def parser2pipeline[T](parse: Parser[T]): Pipeline[Input, T] = new Pipeline[Input, T] {

        def run(v: Input) = parse(v) match {
            case Success(trees, _) => trees
            case e =>
                throw new PipelineError(s"Parser in pipeline failed with error : $e")
        }
    }

    /** Implicit conversion from function to pipeline   */
    implicit def funct2pipeline[F, T](func: Function1[F, T]): Pipeline[F, T] = new Pipeline[F, T] {
        def run(v: F): T = func(v)
    }

    def apply[F, T](func: Function1[F, T]): Pipeline[F, T] = implicitly(func)

    /**
      * Function to repeat a pipeline while a function is true about the two last elements
      */
    def repeatWhile[T](pipe: Pipeline[T, T], whileCond: Function2[T, T, Boolean]): Pipeline[T, T] =
        new Pipeline[T, T] {
            @tailrec def run(v: T): T = {
                val res = pipe.run(v)
                if (whileCond(v, res)) run(res) else v
            }
        }

    /**
      * Function to repeat a pipeline while something is true about the last element
      */
    def repeatWhile[T](pipe: Pipeline[T, T], whileCond: Function1[T, Boolean]): Pipeline[T, T] =
        repeatWhile(pipe, (a: T, b: T) => whileCond(b))
}

