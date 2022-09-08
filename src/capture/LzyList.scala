package capture

import scala.compiletime.uninitialized
import LzyList.{Nil, #:}

trait LzyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: {this} LzyList[A]
}

object LzyList {
  object Nil extends LzyList[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: () => {*} LzyList[A]) extends LzyList[A] {
    private var forced = false
    private var cache: {this} LzyList[A] = uninitialized
    private def force = {
      if !forced then { cache = tl(); forced = true }
      cache
    }
    def isEmpty = false
    def head = hd
    def tail: {this} LzyList[A] = force
  }

  extension [A](x: A)
    def #:(xs1: => {*} LzyList[A]): {xs1} LzyList[A] =
      Cons(x, () => xs1)

  extension [A](xs: {*} LzyList[A]) {
    def filter(p: A => Boolean): {xs, p} LzyList[A] =
      if xs.isEmpty then Nil
      else if p(xs.head) then xs.head #: xs.tail.filter(p)
      else xs.tail.filter(p)

    def take(n: Int): {xs} LzyList[A] = {
      if (n <= 0 || xs.isEmpty) Nil
      else if (n == 1) xs.head #: Nil
      else xs.head #: xs.tail.take(n - 1)
    }

    def force: {xs} LzyList[A] = {
      var these = xs
      while (!these.isEmpty) these = these.tail
      xs
    }
  }

  def from(start: Int, step: Int): LzyList[Int] =
    start #: from(start + step, step)

  def from(start: Int): LzyList[Int] = from(start, 1)
}
