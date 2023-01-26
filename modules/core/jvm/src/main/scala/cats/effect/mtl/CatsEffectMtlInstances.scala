package cats.effect.mtl

import cats.effect.Ref
import cats.mtl.{Ask, Stateful, Tell}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, Semigroup}

object CatsEffectMtlInstances:

  class RefStateful[F[_]: Monad, S](ref: Ref[F, S]) extends Stateful[F, S]:
    val monad: Monad[F]                      = implicitly
    def get: F[S]                            = ref.get
    def set(s: S): F[Unit]                   = ref.set(s)
    override def inspect[A](f: S => A): F[A] = ref.get.map(f)
    override def modify(f: S => S): F[Unit]  = ref.update(f)

  class RefTell[F[_]: Functor, L: Semigroup](ref: Ref[F, L]) extends Tell[F, L]:
    val functor: Functor[F] = implicitly
    def tell(l: L): F[Unit] = ref.update(_ |+| l)

  class RefAsk[F[_]: Applicative, S](ref: Ref[F, S]) extends Ask[F, S]:
    val applicative: Applicative[F]  = implicitly
    override def ask[E2 >: S]: F[E2] = ref.get.map(implicitly[S <:< E2])
end CatsEffectMtlInstances
