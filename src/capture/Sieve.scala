package capture

import Stream.{#::, from}

@main def Sieve(names: String*) = {
  lazy val ps: Stream[Int] = 2 #:: from(3).filter(i => ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
  println(ps.drop(10000).head)
}
