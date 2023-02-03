package fennec

import cats.effect.kernel.{Clock, Concurrent, Ref}
import cats.FlatMap
import cats.syntax.all.*
import cats.effect.std.UUIDGen

import java.util.UUID
import cats.Functor

import scala.annotation.nowarn

case class FennecSession[+State, +User](
    session: KernelSession[User],
    state: State,
    sessionCounter: Long,
    lastUserEventEpochMillis: Long,
):
  def incrementSession = copy(sessionCounter = this.sessionCounter + 1)

trait Session[F[_]: Functor, State, User]:
  def incrementCounter: F[Long]
  def setSession(session: KernelSession[User]): F[Unit]
  def setState(state: State): F[Unit]
  def updateState(f: State => State): F[Unit]
  def get: F[FennecSession[State, User]]
  def resetLastUserEventTime: F[Unit]
  def id: F[UUID] = get.map(_.session.id)

object Session:

  def apply[F[_]: Clock: FlatMap, State, User](st: Ref[F, FennecSession[State, User]]) = new Session[F, State, User]:
    def incrementCounter: F[Long]                         = st.getAndUpdate(_.incrementSession).map(_.sessionCounter)
    def setSession(session: KernelSession[User]): F[Unit] = st.update(_.copy(session = session))
    def setState(state: State): F[Unit]                   = st.update(_.copy(state = state))
    def get: F[FennecSession[State, User]]                = st.get
    def updateState(f: State => State): F[Unit]           = st.update(sess => sess.copy(state = f(sess.state)))
    def resetLastUserEventTime: F[Unit] =
      for
        currentTime <- Clock[F].realTime
        _           <- st.update(_.copy(lastUserEventEpochMillis = currentTime.toMillis))
      yield ()

  @nowarn("msg=unused implicit parameter")
  def make[F[_]: UUIDGen: Concurrent: Clock: FlatMap, State, User](
      state: State,
      maybeKernelSession: Option[KernelSession[User]],
  ): F[Session[F, State, User]] =
    for
      currentTime   <- Clock[F].realTime
      kernelSession <- maybeKernelSession.fold(UUIDGen[F].randomUUID.map(KernelSession.AnonymousSession(_)))(_.pure[F])
      st <- Ref[F].of(
        FennecSession[State, User](
          kernelSession,
          state,
          0,
          currentTime.toMillis,
        ),
      )
    yield apply(st)

end Session
