package capture

import scala.annotation.tailrec
import LzyList.{Nil, #:}

trait LzyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Lazy[LzyList[A]]

  def filter(p: A -> Boolean): LzyList[A] =
    if isEmpty then Nil
    else if p(head) then head #: tail.map(_.filter(p))
    else tail.apply.filter(p)

  def take(n: Int): LzyList[A] =
    if (n <= 0 || isEmpty) Nil
    else if (n == 1) head #: Lazy(Nil)
    else head #: tail.map(_.take(n - 1))

  @tailrec final def drop(n: Int): {this} LzyList[A] =
    if (n <= 0 || isEmpty) this
    else tail.apply.drop(n - 1)

  def force: {this} LzyList[A] =
    var these = this
    while (!these.isEmpty) these = these.tail.apply
    this
}

object LzyList {
  object Nil extends LzyList[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: Lazy[LzyList[A]]) extends LzyList[A] {
    def isEmpty = false
    def head = hd
    def tail = tl
  }

  extension [A](x: A)
    def #:(xs1: Lazy[LzyList[A]]): LzyList[A] =
      Cons(x, xs1)

  def from(start: Int, step: Int): LzyList[Int] =
    start #: Lazy(from(start + step, step))

  def from(start: Int): LzyList[Int] = from(start, 1)
}
