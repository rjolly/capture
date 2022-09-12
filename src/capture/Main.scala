package capture

@main def Main(names: String*) = {
  def primes = sieve(Stream.from(2))
  def sieve(s: Stream[Int]): Stream[Int] = {
    val n = s.head
    Stream.cons(n, sieve(s.tail filter { _ % n != 0 }))
  }
  primes.take(4000).force
  println("hello world")
}
