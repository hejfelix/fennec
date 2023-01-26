package fennec

import cats.{~>, Applicative, Id}

object KernelCatsSupport:

  given [F[_]]: (F ~> F) with
    def apply[A](fa: F[A]): F[A] = fa

  given [F[_]: Applicative]: (Id ~> F) with
    def apply[A](fa: Id[A]): F[A] = Applicative[F].pure(fa)

end KernelCatsSupport
