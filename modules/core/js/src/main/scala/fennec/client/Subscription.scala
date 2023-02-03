package fennec.client

import cats.syntax.all.*
import cats.{Functor, FunctorFilter}
import fs2.Stream
import fs2.concurrent.Topic

case class Subscription[F[_], A](t: () => Stream[F, A]) {
  def collect[B](f: PartialFunction[A, B]) = Subscription(t.map(_.collect(f)))
}

object Subscription:

  def apply[F[_], A](t: Topic[F, A]): Subscription[F, A] = Subscription(() => t.subscribe(10))
  def const[F[_], A](a: A): Subscription[F, A]           = Subscription(() => Stream.emit(a))


  given [F[_]]: FunctorFilter[[A] =>> Subscription[F, A]] with
    val functor: Functor[[A] =>> Subscription[F, A]] = summon
    def mapFilter[A, B](fa: Subscription[F, A])(f: A => Option[B]): Subscription[F, B] =
      Subscription(fa.t.map(_.map(f).flattenOption))

  given [F[_]]: Functor[[A] =>> Subscription[F, A]] with
    def map[A, B](fa: Subscription[F, A])(f: A => B): Subscription[F, B] = Subscription(fa.t.map(_.map(f)))

  extension [F[_], A](s: Subscription[F, A]) def run: Stream[F, A] = s.t()

end Subscription
