package fennec.server

import fennec.Codec

import fs2.Stream

abstract class MessageQueue[F[_], T: Codec]:
  val publish: T => F[Unit]
  val receive: Stream[F, T]
