package capture

import language.experimental.saferExceptions
import LzyList.{Nil, #:, from}

@main def Main(names: String*) = {
  class ArithmeticEx extends Exception
  def mod(x: Int, y: Int): Int throws ArithmeticEx = x % y
  def primes = sieve(from(2))
  def sieve(s: {*} LzyList[Int]): {s} LzyList[Int] = {
    val n = s.head
    try n #: sieve(s.tail filter { mod(_, n) != 0 })
    catch case ex: ArithmeticEx => Nil
  }
  var t = System.currentTimeMillis();
  val r = primes.drop(4000).head
  t = System.currentTimeMillis() - t;
  println(s"r: ${r}, t: ${t}")
}
