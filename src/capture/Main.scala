package capture

import LzyList.{Nil, filter, force, #:, range}

@main def Main(names: String*) = {
  val n = 4000
  def primes = sieve(range(2, n, 1))
  def sieve(s: LzyList[Int]): LzyList[Int] = s match {
    case Nil => Nil
    case _ => s.head #: sieve(s.tail filter { _ % s.head != 0 })
  }
  primes.force
  println("hello world")
}
