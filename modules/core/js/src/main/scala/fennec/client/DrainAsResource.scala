package fennec.client

import cats.effect.{Concurrent, Resource, Fiber}
import fs2.Stream
import cats.effect.syntax.all.*

extension [F[_]: Concurrent, S](s: Stream[F, S])
  def drainAsResource: Resource[F, Fiber[F, Throwable, Unit]] =
    Resource.make(s.compile.drain.start)(_.cancel)
