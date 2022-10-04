package capture

import LzyList.{#:, from}

@main def Main(names: String*) = {
  def primes = sieve(from(2))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = {
    val n = s.head
    n #: s.tail.map(s => sieve(s filter { _ % n != 0 }))
  }
  for (i <- 1 to 10)
    var t = System.currentTimeMillis();
    val r = primes.drop(i * 4000).head
    t = System.currentTimeMillis() - t;
    println(s"r: ${r}, t: ${t}")
}
