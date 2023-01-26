package fennec

import java.util.UUID

import cats.*
import cats.syntax.all.*

import Kernel.*

final case class Kernel[F[_]: Applicative, State: Codec, Event: Codec, User: Codec](
    update: Update[State, Event],
    effect: UpdateEffect[F, State, Event, User],
    sharedQueues: List[SharedQueue],
    sharedQueueFor: Event => Option[SharedQueue],
    name: String,
    initState: State,
    authenticate: Proof => F[Either[Throwable, User]],
):

  def resetAuthentication[U: Codec](using ev: User =:= Unit): Kernel[F, State, Event, U] =
    copy(
      effect = (session: KernelSession[U]) => s => e => effect(session.map(_ => ev.flip.apply(())))(s)(e),
      authenticate = _ => (new Exception("no authentication implemented"): Throwable).asLeft[U].pure[F],
    )

  def withAuthentication(authenticateU: Proof => F[Either[Throwable, User]]): Kernel[F, State, Event, User] =
    copy(authenticate = authenticateU)

  def covary[G[_]: Applicative](using fg: F ~> G): Kernel[G, State, Event, User] =
    copy(
      effect = effect.andThen(_.andThen(_.andThen(fg.apply))),
      authenticate = authenticate.andThen(fg.apply),
    )

  def withQueueMapping(mapping: PartialFunction[Event, SharedQueue]): Kernel[F, State, Event, User] =
    copy(sharedQueueFor = e => sharedQueueFor(e).orElse(mapping.lift(e)))

  def withSharedQueues(qs: SharedQueue*): Kernel[F, State, Event, User] = copy(sharedQueues = qs.toList ++ sharedQueues)

  def withEffect[G[_]: Monad](
      eff: UpdateEffect[G, State, Event, User],
  )(using fg: F ~> G): Kernel[G, State, Event, User] =
    copy(
      authenticate = authenticate.andThen(fg.apply),
      effect = (session: KernelSession[User]) =>
        (s: State) =>
          (e: Event) =>
            for
              newEvents     <- fg(effect(session)(s)(e))
              moreNewEvents <- eff(session)(s)(e)
            yield newEvents ++ moreNewEvents,
    )

  def withUpdate(u: Update[State, Event]): Kernel[F, State, Event, User] =
    copy(update = (s: State) => (e: Event) => u(update(s)(e))(e))

  type S       = State
  type E       = Event
  type M       = Message[State, Event, User]
  type U       = User

  val eventCodec: Codec[Event] = Codec[Event]
  val stateCodec: Codec[State] = Codec[State]
  val messageCodec: Codec[M]   = CustomCodecSupport.messageCodec[State, Event, User]

end Kernel

object Kernel:

  type KernelA[F[_], State, Event] = Kernel[F, State, Event, Unit]

  def init[State: Codec, Event: Codec](name: String, init: State)(using Codec[Unit]): Kernel[Id, State, Event, Unit] =
    Kernel(
      update = s => _ => s,
      effect = _ => _ => _ => List.empty,
      sharedQueues = Nil,
      sharedQueueFor = _ => None,
      name = name,
      initState = init,
      authenticate = proof => ().asRight,
    )

  case class SharedQueue(name: String)

end Kernel
