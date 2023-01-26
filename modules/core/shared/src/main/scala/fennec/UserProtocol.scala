package fennec

import cats.data.Kleisli
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import fs2.Stream
import org.legogroup.woof.{Logger, given}
import fs2.concurrent.Topic
import cats.Applicative

class UserProtocol[F[_]: Concurrent: Logger, State, Event, User](val kernel: Kernel[F, State, Event, User]):

  type E  = Event
  type M  = Message[State, Event, User]
  type FS = FennecSession[State, User]

  def update(using s: Session[F, State, User]): M => F[Unit] = m =>
    m match
      case Message.EventMessage(_, event) =>
        for
          _ <- s.updateState(state => kernel.update(state)(event))
          _ <- s.incrementCounter
        yield ()
      case Message.SharedEvent(origin, event) =>
        for
          id <- s.get.map(_.session.id)
          _  <- if id != origin then s.updateState(state => kernel.update(state)(event)) else Applicative[F].unit
          _  <- s.incrementCounter
        yield ()
      case Message.SessionHandshake(state,id) =>
        for 
          _ <- s.setState(state)
        yield ()
      case _ => Applicative[F].unit

  def effect(using s: Session[F, State, User]): Event => F[List[M]] =
    event =>
      for
        st <- s.get
        newEvents <- kernel
          .effect(st.session)(st.state)(event)
          .map(
            _.zipWithIndex.map((event, index) => Message.EventMessage(index + st.sessionCounter, event)),
          )
          .onError(e => Logger[F].error(e.toString))
          .recoverWith(_ => List.empty.pure)
      yield newEvents

  def effectM(using s: Session[F, State, User]): M => F[List[M]] = {
    case Message.EventMessage(_, event) => effect(event)
    case Message.SharedEvent(origin, event) =>
      for
        id        <- s.id
        newEvents <- if id != origin then effect(event) else List.empty.pure
      yield newEvents
    case _ => List.empty.pure
  }

  def step(using Session[F, State, User]): M => F[List[M]] =
    m =>
      for
        _         <- update(m)
        newEvents <- effectM(m)
      yield newEvents

  def handleMessage(using s: Session[F, State, User]): M => Stream[F, List[M]] = m =>
    Stream.unfoldEval(List(m)) {
      case m :: ms =>
        step(m)
          .map(nextMessages =>
            val allMessages = ms ++ nextMessages
            Some((allMessages, allMessages)),
          )
      case Nil => None.pure
    }

  def handleF(using Session[F, State, User]): (M => F[Unit]) => M => F[Unit] =
    emit =>
      m =>
        for
          newMessages <- step(m)
          _           <- newMessages.traverse(emit)
        yield ()

  def topic: F[Topic[F, M]] = Topic[F, M]

end UserProtocol
