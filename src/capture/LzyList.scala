package capture

import LzyList.{Nil, #:}

trait LzyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: {this} LzyList[A]

  def filter(p: A -> Boolean): {this} LzyList[A] =
    var rest = this
    while !rest.isEmpty && !p(rest.head) do rest = rest.tail
    if !rest.isEmpty then rest.head #: rest.tail.filter(p)
    else Nil

  def take(n: Int): {this} LzyList[A] =
    if (n <= 0 || isEmpty) Nil
    else if (n == 1) head #: Nil
    else head #: tail.take(n - 1)

  def force: {this} LzyList[A] =
    var these = this
    while (!these.isEmpty) these = these.tail
    this
}

object LzyList {
  object Nil extends LzyList[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: () => {*} LzyList[A]) extends LzyList[A] {
    def isEmpty = false
    def head = hd
    def tail: {this} LzyList[A] = tl()
  }

  extension [A](x: A)
    def #:(xs1: => {*} LzyList[A]): {xs1} LzyList[A] =
      Cons(x, () => xs1)

  def from(start: Int, step: Int): LzyList[Int] =
    start #: from(start + step, step)

  def from(start: Int): LzyList[Int] = from(start, 1)
}
