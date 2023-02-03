package fennec.server

import fs2.Stream

abstract class MessageQueue[F[_], T]:
  val publish: T => F[Unit]
  val receive: Stream[F, T]
