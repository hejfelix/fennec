package fennec.server

import fennec.*

import java.util.UUID
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.mtl.syntax.all.*
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.Applicative
import fs2.{Pipe, Stream}
import org.legogroup.woof.{*, given}
import cats.effect.kernel.Clock

import scala.annotation.nowarn

@nowarn("msg=unused import")
class ServerProtocol[F[_]: Concurrent: Clock: Logger: UUIDGen, State, Event, User](
    val kernel: Kernel[F, State, Event, User],
    userProtocol: UserProtocol[F, State, Event, User],
    emit: Message[State, Event, User] => F[Unit], // interleave this message in incoming stream
):

  type K        = Kernel[F, State, Event, User]
  type M        = Message[State, Event, User]
  type FSession = FennecSession[State, User]
  type StateMap = ServerProtocol.StateMap[State, User]

  import kernel.{U, S, E}
  sealed trait UserProtocolStep:
    val state: State
    val generated: List[Event]

  case class Step(state: State, event: Event, generated: List[Event]) extends UserProtocolStep
  case class Start(state: State) extends UserProtocolStep:
    override val generated: List[Event] = List.empty

  case class FennecState(state: State, sessionCounter: Int)

  def protocol(using S: Stateful[F, StateMap]): F[Pipe[F, (kernel.M, Direction), FSession]] =
    for given Session[F, S, U] <- createNewAnonymousSession
    yield messages => handleMessages(messages)

  def sendAck(m: M): F[Unit] = m match
    case Message.EventMessage(i, _) =>
      for
        _ <- emit(Message.Acknowledge(i))
        _ <- Logger[F].debug(s"Sending ack $i")
      yield ()
    case _ => Applicative[F].unit

  def handleMessages(s: Stream[F, (M, Direction)])(using
      S: Stateful[F, StateMap],
      session: Session[F, S, U],
  ): Stream[F, FSession] =
    s.evalMap((m, dir) =>
      for
        _ <- handleSessionRequests(m, session)
        _ <- userProtocol.handleF(emit)(m) <* (if dir == Direction.Incoming then sendAck(m)
                                               else Applicative[F].unit)
        st <- summon[Session[F, State, User]].get
        _  <- S.modify(_.set(st.session.id, st.state))
      yield st,
    )

  def handleSessionRequests(m: M, existingSession: Session[F, S, U])(using
      S: Stateful[F, StateMap],
  ): F[Unit] =
    import fennec.Message.*
    m match
      case RequestSession(None) =>
        for
          id <- existingSession.get.map(_.session.id)
          _  <- emit(Message.SessionHandshake(kernel.initState, id))
        yield ()
      case RequestSession(Some(id)) =>
        for
          info <- S.get.map(_.find(id))
          _    <- Logger[F].debug(s"Found $info for id $id")
          _ <- info.fold(Applicative[F].unit)((kernelSession, state) =>
            for
              _ <- existingSession.setState(state)
              _ <- existingSession.setSession(kernelSession)
            yield (),
          )
          state <- existingSession.get.map(_.state)
          sid   <- existingSession.id
          _     <- emit(Message.SessionHandshake(state, sid))
        yield ()
      case _ => Applicative[F].unit
  end handleSessionRequests

  def createNewAnonymousSession(using S: Stateful[F, StateMap]): F[Session[F, State, User]] =
    for
      session <- fennec.Session.make[F, State, User](kernel.initState, None)
      id      <- session.get.map(_.session.id)
      _       <- Logger[F].info(s"[${kernel.name}] Hello, $id")
      _       <- S.modify(_.createAnonymous(id, kernel.initState))
    yield session

end ServerProtocol

object ServerProtocol:

  case class StateMap[State, User](m: Map[KernelSession[User], State]):
    def exists(id: UUID): Boolean                            = find(id).isDefined
    def find(id: UUID): Option[(KernelSession[User], State)] = m.find(_._1.id == id)
    def findSession(id: UUID): Option[KernelSession[User]]   = find(id).map(_._1)
    def findState(id: UUID): Option[State]                   = find(id).map(_._2)
    def set(id: UUID, state: State): StateMap[State, User] =
      findSession(id).map(s => copy(m.updated(s, state))).getOrElse(this)
    def createAnonymous(id: UUID, s: State) = StateMap(
      m.updated(KernelSession.AnonymousSession(id), s),
    )
  object StateMap:
    def empty[S, U] = StateMap[S, U](Map.empty)

end ServerProtocol
