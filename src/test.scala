import language.experimental.captureChecking

@annotation.capability class Cap

class LazyRef[T](val elem: () => T):
  val get: {elem} () -> T = elem
  def map[U](f: T => U): {f, this} LazyRef[U] =
    new LazyRef(() => f(elem()))

def map[A, B](ref: {*} LazyRef[A], f: A => B): {f, ref} LazyRef[B] =
  new LazyRef(() => f(ref.elem()))

def mapd[A, B]: ({*} LazyRef[A], A => B) => {*} LazyRef[B] =
   (ref1, f1) => map[A, B](ref1, f1)
