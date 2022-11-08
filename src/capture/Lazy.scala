package capture

import language.experimental.captureChecking

trait Lazy[+T] extends (() => T):
  def map[S](f: T -> S) = Lazy(f(apply))
  def flatMap[S](f: T -> Lazy[S]) = f(apply)

object Lazy:
  def apply[T](body: => T): {body} Lazy[T] = new Lazy[T]:
    lazy val apply = body
