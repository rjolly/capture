package capture

@main def Main(names: String*) = {
  def primes = sieve(Stream.from(2))
  def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter { _ % s.head != 0 })
  primes.take(4000).force
  println("hello world")
}
