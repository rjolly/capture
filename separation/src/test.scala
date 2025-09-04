import language.experimental.captureChecking
import java.io.FileOutputStream

def usingLogFile[T](op: FileOutputStream^ => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

@main def Main(names: String*) = {
  val later = usingLogFile { file => () => file.write(0) }
  later() // crash
}
