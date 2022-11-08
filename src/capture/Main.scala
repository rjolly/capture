package capture

import language.experimental.captureChecking
import LzyList.{#:, range}

@main def Main(names: String*) = {
  val m = Array(37831, 81817, 128201, 176087, 224743, 274579, 324953, 376133, 427993, 479939)
  def primes(max: Int) = sieve(range(2, max + 1))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = {
    val n = s.head
    n #: s.tail.map(s => sieve(s filter { _ % n != 0 }))
  }
  for (i <- 1 to 10)
    var t = System.currentTimeMillis();
    val r = primes(m(i - 1)).drop(i * 4000).head
    t = System.currentTimeMillis() - t;
    println(s"r: ${r}, t: ${t}")
}
