import language.experimental.{captureChecking, separationChecking}
import scala.caps.Mutable

type Matrix = Array[Int]

def setElem(consume a: Matrix^, i: Int, j: Int, x: Double): Unit = ???
def getElem(a: Matrix, i: Int, j: Int): Double = ???

def multiply(a: Matrix, b: Matrix, consume c: Matrix^): Unit =
  setElem(c, 0, 0, 0)

@main def Main(names: String*) = ()

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
//def foo(x: Int) = set(x)
//                  ^^^
//                  cannot call update method set from (Ref.this : Ref),
//                  since its capture set {Ref.this} is read-only
