package capture

import language.experimental.captureChecking

trait Lazy[+T] extends (() => T):
  def map[S](f: T -> S): Lazy[S]^{this} = Lazy(f(apply()))
  def flatMap[S](f: T -> Lazy[S]) = f(apply())

object Lazy:
  def apply[T](body: => T): Lazy[T]^{body} = new Lazy[T]:
    lazy val apply = body
