package capture

import LzyList.{#:, from}

@main def Main(names: String*) = {
  def primes = sieve(from(2))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = {
    val n = s.head
    n #: sieve(s.tail filter { _ % n != 0 })
  }
  var t = System.currentTimeMillis();
  val r = primes.drop(4000).head
  t = System.currentTimeMillis() - t;
  println(s"r: ${r}, t: ${t}")
}
