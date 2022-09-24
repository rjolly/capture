package capture

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

given ExecutionContext = ExecutionContext.global

type Future[T] = scala.concurrent.Future[T]

val Future = scala.concurrent.Future

extension[T](s: Future[T])
  def await = Await.result(s, Duration.Inf)
