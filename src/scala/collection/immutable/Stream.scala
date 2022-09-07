package scala.collection.immutable

import scala.annotation.unchecked.uncheckedVariance

abstract class Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]
  def force: Stream[A]

  def take(n: Int): Stream[A] = {
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) new Stream.Cons(head, Stream.empty)
    else new Stream.Cons(head, tail.take(n - 1))
  }

  def filter(p: A => Boolean): Stream[A] = {
    var rest: Stream[A] = this
    while (!rest.isEmpty && p(rest.head) == false) rest = rest.tail
    if (!rest.isEmpty) Stream.filteredTail(rest, p)
    else Stream.empty
  }

  protected def tailDefined: Boolean
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
    protected def tailDefined: Boolean = false
  }

  final class Cons[A](override val head: A, tl: => Stream[A]) extends Stream[A] {
    def isEmpty: Boolean = false
    @volatile private[this] var tlVal: Stream[A] = _
    protected def tailDefined: Boolean = tlVal ne null
    def tail: Stream[A] = {
      if (!tailDefined)
        synchronized {
          if (!tailDefined) tlVal = tl
        }
      tlVal
    }
    def force = {
      // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
      var these, those: Stream[A] = this
      if (!these.isEmpty) these = these.tail
      while (those ne these) {
        if (these.isEmpty) return this
        these = these.tail
        if (these.isEmpty) return this
        these = these.tail
        if (these eq those) return this
        those = those.tail
      }
      this
    }
  }

  def empty[A]: Stream[A] = Empty

  def from(start: Int, step: Int): Stream[Int] =
    cons(start, from(start + step, step))

  def from(start: Int): Stream[Int] = from(start, 1)

  private[Stream] def filteredTail[A](stream: Stream[A] @uncheckedVariance, p: A => Boolean) = {
    cons(stream.head, stream.tail.filter(p))
  }
}
