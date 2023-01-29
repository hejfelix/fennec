package fennec.client

import cats.effect.std.Dispatcher
import cats.effect.std.UUIDGen
import org.legogroup.woof.Logger
import cats.effect.Concurrent
import fennec.Kernel
import java.util.UUID
import org.scalajs.dom
import cats.effect.kernel.Resource
import fs2.concurrent.Topic
import fennec.FennecSession
import fennec.UserProtocol
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, byteArray2Int8Array}
import org.scalajs.dom.MessageEvent
import Websocket.WebsocketChannel
import fs2.Stream
import cats.syntax.all.*
import fennec.Direction
import fennec.Message
import cats.Applicative
import fennec.Session
import cats.Monad

object KernelSocket {

  def toM[F[_]: Async](kernel: Kernel[F, ?, ?, ?])(me: dom.MessageEvent): F[kernel.M] =
    val blob = me.data.asInstanceOf[org.scalajs.dom.Blob]
    Async[F].fromFuture(
      Sync[F].delay(
        blob
          .arrayBuffer()
          .`then` { (x: ArrayBuffer) =>
            val uint8Array = new Int8Array(x)
            val decoded: Either[Throwable, (Vector[Byte], kernel.M)] =
              kernel.messageCodec.decode.run(uint8Array.toVector)
            val (remaining, message: kernel.M) =
              decoded.fold(throwable => throw throwable, identity)
            message
          }
          .toFuture,
      ),
    )

  def fromM[F[_]](kernel: Kernel[F, ?, ?, ?])(m: kernel.M): dom.MessageEvent =
    val pickled = kernel.messageCodec.encode(m)
    val arr     = pickled.toArray
    val ia      = new Int8Array(pickled.size)
    ia.set(byteArray2Int8Array(arr))
    val event = new dom.MessageEventInit {}
    event.data = ia
    dom.MessageEvent(
      "message",
      event,
    )

  def topicFor[F[_]: Async: UUIDGen: Logger, S, E, U](
      channel: WebsocketChannel[F],
      kernel: Kernel[F, S, E, U],
      id: UUID,
  ): Resource[F, (Topic[F, kernel.E], Subscription[F, FennecSession[kernel.S, kernel.U]])] =
    val userProtocol = UserProtocol(kernel)
    for
      eventTopic             <- Resource.eval(Topic[F, kernel.E])
      eventStream            <- eventTopic.subscribeAwait(10)
      outgoingTopicM         <- Resource.eval(Topic[F, kernel.M])
      outgoingKernelMessages <- outgoingTopicM.subscribeAwait(10)
      incomingStream         <- channel.in.subscribeAwait(10)
      incomingKernelMessages: Stream[F, kernel.M] = incomingStream.evalMap(toM(kernel))
      allMessages = Stream(
        outgoingKernelMessages.product(Direction.Outgoing.pure),
        incomingKernelMessages.product(Direction.Incoming.pure),
        eventStream.map(e => Message.EventMessage(-1, e)).product(Direction.Outgoing.pure),
      ).parJoinUnbounded
        .evalTap {
          case (Message.SessionHandshake(_, id), Direction.Incoming) =>
            Sync[F].delay(
              org.scalajs.dom.window.localStorage.setItem(kernel.name, id.toString),
            )
          case _ => Applicative[F].unit
        }
      session <- Resource.eval(Session.make[F, kernel.S, kernel.U](kernel.initState, None))
      sessionStates: Stream[F, FennecSession[kernel.S, kernel.U]] = allMessages.evalMap((m, dir) =>
        handleSessionStates(kernel)((m, dir), session, outgoingTopicM.publish1(_).void, channel),
      )
      _                  <- Resource.eval(outgoingTopicM.publish1(Message.RequestSession(Some(id))))
      sessionStatesTopic <- Resource.eval(Topic[F, FennecSession[kernel.S, kernel.U]])
      _ <- sessionStates
        .through(sessionStatesTopic.publish)
        .drainAsResource // start pulling the whole system
    yield (eventTopic, Subscription(sessionStatesTopic))
  end topicFor

  def handleSessionStates[F[_]: Concurrent: Logger, S, U](
      kernel: Kernel[F, S, ?, U],
  )(
      m: (kernel.M, Direction),
      session: Session[F, kernel.S, kernel.U],
      publishM: kernel.M => F[Unit],
      channel: WebsocketChannel[F],
  ): F[FennecSession[S, U]] =
    val userProtocol = UserProtocol(kernel)
    m match {
      case (m @ Message.EventMessage(-1, _), Direction.Outgoing) =>
        for
          counter <- session.incrementCounter
          mUpdated = m.copy(sessionCounter = counter)
          _                      <- channel.out.publish1(fromM(kernel)(mUpdated))
          given Session[F, S, U] <- session.pure
          asdf = summon[Session[F, S, U]]
          r     <- userProtocol.handleF(publishM)(mUpdated)
          state <- session.get
        yield state
      case (m, Direction.Outgoing) =>
        for
          _                      <- channel.out.publish1(fromM(kernel)(m))
          given Session[F, S, U] <- session.pure
          _                      <- userProtocol.handleF(publishM)(m)
          state                  <- session.get
        yield state
      case (m, Direction.Incoming) =>
        given Session[F, S, U] = session
        userProtocol.handleF(publishM)(m) *> session.get
    }

}
