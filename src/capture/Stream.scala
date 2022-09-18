package capture

abstract class Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]
  def force: Stream[A]

  def take(n: Int): Stream[A] = {
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) Stream.cons(head, Stream.empty)
    else Stream.cons(head, tail.take(n - 1))
  }

  def filter(p: A => Boolean): Stream[A] = {
    var rest: Stream[A] = this
    while (!rest.isEmpty && p(rest.head) == false) rest = rest.tail
    if (!rest.isEmpty) Stream.cons(rest.head, rest.tail.filter(p))
    else Stream.empty
  }
}

object Stream {
  object cons {
    def apply[A](hd: A, tl: => Stream[A]): Stream[A] = new Cons(hd, tl)
  }

  object Empty extends Stream[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("head of empty stream")
    def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    def force = this
  }

  final class Cons[A](override val head: A, tl: => Stream[A]) extends Stream[A] {
    def isEmpty: Boolean = false
    def tail = tl
    def force = {
      var these: Stream[A] = this
      while (!these.isEmpty) these = these.tail
      this
    }
  }

  def empty[A]: Stream[A] = Empty

  def from(start: Int, step: Int): Stream[Int] =
    cons(start, from(start + step, step))

  def from(start: Int): Stream[Int] = from(start, 1)
}
