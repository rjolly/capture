package capture

import scala.annotation.tailrec

import LzyList.{Nil, #:}

trait LzyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: {this} LzyList[A]

  @tailrec final def filter(p: A -> Boolean): {this} LzyList[A] =
    if isEmpty then Nil
    else if p(head) then head #: tail.filter2(p)
    else tail.filter(p)

  def filter2(p: A -> Boolean): {this} LzyList[A] = filter(p)

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
