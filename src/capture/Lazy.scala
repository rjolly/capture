package capture

import scala.compiletime.uninitialized

trait Lazy[+T] extends (() => T):
  def map[S](f: T -> S) = Lazy(f(apply))
  def flatMap[S](f: T -> Lazy[S]) = f(apply)

object Lazy:
  def apply[T](body: => T): {body} Lazy[T] = new Lazy[T]:
    private var g = () => body
    private var h: T = uninitialized
    def apply =
      if (g ne null)
        h = g()
        g = null
      h
