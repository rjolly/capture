import language.experimental.captureChecking
def f(x: () -> Unit): Unit = x()
class A extends caps.SharedCapability
val a = new A
val t = () => { a; () }
//              ^
//              A pure expression does nothing in statement position
//
val c = f(t)
