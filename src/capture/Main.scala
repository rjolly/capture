package capture

import LzyList.{#:, from}

@main def Main(names: String*) = {
  def primes = sieve(from(2))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = s match
    case head #: tail => head #: sieve(tail filter { _ % head != 0 })
  primes.take(4000).force
  println("hello world")
}
