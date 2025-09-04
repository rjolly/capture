import language.experimental.{captureChecking, separationChecking}

class Matrix(nrows: Int, ncols: Int) extends caps.Mutable:
  update def setElem(i: Int, j: Int, x: Double): Unit = ???
  def getElem(i: Int, j: Int): Double = ???

def multiply(a: Matrix, b: Matrix, c: Matrix^): Unit =
  c.setElem(0, 0, 0)

@main def Main(names: String*) = ()
