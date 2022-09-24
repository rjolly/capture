package capture

import LzyList.{#:, range}

@main def Main(names: String*) = {
  val m = 37831 // 37831, 104743, 163847, 479939
  def primes = sieve(range(2, m + 1))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = {
    val n = s.head
    n #: s.tail.map(s => sieve(s filter { _ % n != 0 }))
  }
  var t = System.currentTimeMillis();
  val r = primes.drop(4000).head
  t = System.currentTimeMillis() - t;
  println(s"r: ${r}, t: ${t}")
}
