package fennec.server

import fs2.{Pull, Stream}

object Utils:

  extension [F[_], A](s: Stream[F, A])
    def scanReset[B](identity: B, f: (B, A) => F[B])(resetIdentity: PartialFunction[A, F[B]]): Stream[F, B] =
      def loop(acc: B, s: Stream[F, A]): Pull[F, B, Unit] =
        s.pull.uncons1.flatMap {
          case Some((head, tail)) =>
            for
              maybeResetAcc <-
                if resetIdentity.isDefinedAt(head) then Pull.eval(resetIdentity(head)) else Pull.pure(acc)
              nextElement   <- Pull.eval(f(maybeResetAcc, head))
              _             <- Pull.output1(nextElement)
              _             <- loop(nextElement, tail)
            yield ()
          case None               => Pull.done
        }
      loop(identity, s).void.stream

  extension [F[_], T](s: Stream[F, T])
    def flatPipe[U](f: ((T, Stream[F, T])) => Stream[F, U]): Stream[F, U] =
      s.pull.uncons1
        .flatMap {
          case Some(x) => f(x).pull.echo
          case None    => Pull.done
        }
        .void
        .stream

end Utils
