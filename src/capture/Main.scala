package capture

import LzyList.{filter, force, take, #:, from}

@main def Main(names: String*) = {
  def primes = sieve(from(2))
  def sieve(s: LzyList[Int]): LzyList[Int] = s.head #: sieve(s.tail filter { _ % s.head != 0 })
  primes.take(4000).force
  println("hello world")
}
