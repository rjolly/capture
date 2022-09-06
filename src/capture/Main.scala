package capture

@main def Main(names: String*) = {
  def primes = sieve(LazyList.from(2))
  def sieve(s: LazyList[Int]): LazyList[Int] = s.head #:: sieve(s.tail filter { _ % s.head != 0 })
  primes.take(4000).force
  println("hello world")
}
