package fos

/**
  * Created by Valentin on 18/09/14.
  */
abstract class Pipeline[-F, +T] {
    self =>

    def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
        def run(v: F): G = {
            val first = self.run(v)
            thenn.run(first)
        }
    }

    def run(v: F): T

    def | = andThen _
}

