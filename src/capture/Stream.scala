package capture

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import Stream.{Cons => cons, Empty, future}

abstract class Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]

  def force: Stream[A] = {
    var these = this
    while (!these.isEmpty) these = these.tail
    this
  }

  def take(n: Int): Stream[A] = this match {
    case cons(head, tail) => if (n <= 0) Stream.empty
      else if (n == 1) cons(head, future(Stream.empty))
      else cons(head, tail.map(_ take n-1))
    case Empty => Empty
  }

  def filter(p: A => Boolean): Stream[A] = {
    var rest = this
    while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
    rest match {
      case cons(head, tail) => cons(head, tail.map(_ filter p))
      case Empty => Empty
    }
  }

  protected def tailDefined: Boolean
}

object Stream {
  object Empty extends Stream[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("head of empty stream")
    def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    protected def tailDefined: Boolean = false
  }

  val #:: = Cons

  case class Cons[+A](hd: A, tl: Future[Stream[A]]) extends Stream[A] {
    private[this] var defined: Boolean = _
    def isEmpty = false
    def head = hd
    protected def tailDefined = defined
    def tail: Stream[A] = {
      defined = true
      Await.result(tl, Duration.Inf)
    }
  }

  def empty[A]: Stream[A] = Empty

  def from(start: Int, step: Int): Stream[Int] = cons(start, future(from(start+step, step)))

  def from(start: Int): Stream[Int] = from(start, 1)

  def future[T](body: => T) = Lazy(body)

  def range(start: Int, end: Int, step: Int): Stream[Int] = {
    if (if (step < 0) start <= end else end <= start) Empty
    else cons(start, future(range(start + step, end, step)))
  }
}
