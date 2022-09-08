package capture

import Stream.{Cons => cons, Empty, #::}
import scala.concurrent.ExecutionContext.Implicits.global

@main def Main(names: String*) = {
  val n = 4000
  def primes = sieve(Stream.range(2, n, 1))
  def sieve(s: Stream[Int]): Stream[Int] = s match {
    case head#::tail => cons(head, tail.map(s => sieve(s filter { _ % head != 0 })))
    case Empty => Empty
  }
  primes.force
  println("hello world")
}
