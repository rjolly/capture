package capture

import scala.collection.immutable.Stream
import Stream.{Cons => cons, Empty, #::}

@main def Main(names: String*) = {
  val n = 4000
  def primes = sieve(Stream.range(2, n, 1))
  def sieve(s: Stream[Int]): Stream[Int] = s match {
    case head#::tail => cons(head, sieve(tail filter { _ % head != 0 }))
    case _ => Empty
  }
  primes.force
  println("hello world")
}
