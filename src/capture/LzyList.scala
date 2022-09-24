package capture

import scala.annotation.tailrec
import LzyList.{Nil, #:}

trait LzyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Future[LzyList[A]]

  def filter(p: A -> Boolean): LzyList[A] =
    if isEmpty then Nil
    else if p(head) then head #: tail.map(_.filter(p))
    else tail.await.filter(p)

  def take(n: Int): LzyList[A] =
    if (n <= 0 || isEmpty) Nil
    else if (n == 1) head #: Future(Nil)
    else head #: tail.map(_.take(n - 1))

  @tailrec final def drop(n: Int): {this} LzyList[A] =
    if (n <= 0 || isEmpty) this
    else tail.await.drop(n - 1)

  def force: {this} LzyList[A] =
    var these = this
    while (!these.isEmpty) these = these.tail.await
    this
}

object LzyList {
  object Nil extends LzyList[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: Future[LzyList[A]]) extends LzyList[A] {
    def isEmpty = false
    def head = hd
    def tail = tl
  }

  extension [A](x: A)
    def #:(xs1: Future[LzyList[A]]): LzyList[A] =
      Cons(x, xs1)

  def from(start: Int, step: Int): LzyList[Int] =
    start #: Future(from(start + step, step))

  def from(start: Int): LzyList[Int] = from(start, 1)

  def range(start: Int, end: Int, step: Int): LzyList[Int] =
    if (if (step < 0) start <= end else end <= start) Nil
    else start #: Future(range(start + step, end, step))

  def range(start: Int, end: Int): LzyList[Int] = range(start, end, 1)
}
