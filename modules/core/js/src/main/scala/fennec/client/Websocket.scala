package fennec.client

import fennec.*

import java.util.UUID
import cats.Applicative
import cats.effect.{Async, Concurrent}
import cats.effect.kernel.{Resource, Sync}
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import org.scalajs.dom

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, byteArray2Int8Array}
import scala.scalajs.js.{defined, typedarray}
import scala.util.{Success, Try}
import org.legogroup.woof.{*, given}
import cats.effect.syntax.all.*
import Websocket.*
import fs2.concurrent.Topic
import org.scalajs.dom.MessageEvent
import fs2.Stream

import scala.concurrent.duration.*
import cats.effect.kernel.GenTemporal

import scala.concurrent.Await
import org.scalajs.dom.WebSocket
import cats.Functor
import cats.effect.std.UUIDGen

import javax.script.SimpleBindings
import cats.effect.kernel.Fiber
import cats.effect.kernel.Clock

import scala.scalajs.js
object Websocket:

  private def createWebsocket[F[_]: Async](webSocketUrl: String): F[dom.WebSocket] =
    Async[F].async[dom.WebSocket] { complete =>
      Sync[F].delay {
        println(s"CONNECTING TO $webSocketUrl")
        val websocket = new dom.WebSocket(webSocketUrl)

        websocket.onopen = _ => complete(websocket.asRight)
        websocket.onerror =
          error => complete(Exception(s"websocket error:${error} $webSocketUrl").asLeft)
        websocket.onclose = reason => {
          println(reason)
          complete(Exception(s"websocket closed: $reason").asLeft)
        }

        Some(Sync[F].delay(websocket.close()))
      }
    }

  private def websocketResource[F[_]: Async](webSocketUrl: String): Resource[F, dom.WebSocket] =
    Resource.make(createWebsocket(webSocketUrl))(socket => Sync[F].delay(socket.close()))

  def incoming[F[_]: Async: Concurrent](websocket: WebSocket)(using dispatcher:Dispatcher[F]): Resource[F, Topic[F, MessageEvent]] =
    for
      incomingTopic <- Resource.make(Topic[F, MessageEvent])(_.closed)
      _ <- Resource.make(
        Sync[F].delay(websocket.onmessage =
          m => dispatcher.unsafeRunAndForget(incomingTopic.publish1(m)),
        ),
      )(_ => Sync[F].delay(websocket.onmessage = null))
    yield incomingTopic

  /** A topic which immediately consumes its messages by sending them through a websocket
    */
  def outgoing[F[_]: Logger: Async: Concurrent](
      websocket: WebSocket,
  ): Resource[F, Topic[F, MessageEvent]] =
    for
      outgoingTopic  <- Resource.make(Topic[F, MessageEvent])(_.closed)
      outgoingStream <- outgoingTopic.subscribeAwait(10)
      _ <- Resource.make {
        outgoingStream
          .evalTap(m => Sync[F].delay(websocket.send(m.data.asInstanceOf[Int8Array].buffer)))
          .compile
          .drain
          .start
      }(_.cancel)
    yield outgoingTopic

  def topic[F[_]: Logger: Async: Concurrent: Dispatcher](
      websocketUrl: String,
  ): Resource[F, (Topic[F, MessageEvent], Topic[F, MessageEvent])] =
    for
      socket   <- websocketResource(websocketUrl)
      incoming <- incoming(socket)
      outgoing <- outgoing(socket)
    yield (incoming, outgoing)

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
    val event = new dom.MessageEventInit{}
    event.data = ia
    dom.MessageEvent(
      "message",
      event,
    )

  def topicFor[F[_]: Dispatcher: Clock: UUIDGen: Logger: Async: Concurrent, S, E, U](
      kernel: Kernel[F, S, E, U],
      websocketUrl: String,
      id: UUID,
  ): Resource[F, (Topic[F, kernel.E], Subscription[F, FennecSession[kernel.S, kernel.U]])] =
    val userProtocol = UserProtocol(kernel)
    for
      (incomingTopic, outgoingTopic) <- topic[F](websocketUrl)
      eventTopic                     <- Resource.eval(Topic[F, kernel.E])
      eventStream                    <- eventTopic.subscribeAwait(10)
      outgoingTopicM                 <- Resource.eval(Topic[F, kernel.M])
      outgoingKernelMessages         <- outgoingTopicM.subscribeAwait(10)
      incomingStream                 <- incomingTopic.subscribeAwait(10)
      incomingKernelMessages: Stream[F, kernel.M] = incomingStream.evalMap(toM(kernel))
      allMessages = Stream(
        outgoingKernelMessages.product(Direction.Outgoing.pure),
        incomingKernelMessages.product(Direction.Incoming.pure),
        eventStream.map(e => Message.EventMessage(-1, e)).product(Direction.Outgoing.pure),
      ).parJoinUnbounded
        // .evalTap((m, dir) =>
        //   Logger[F].info(s"${if dir == Direction.Outgoing then "O >>>" else "I <<<"} $m"),
        // )
        .evalTap {
          case (Message.SessionHandshake(_, id), Direction.Incoming) =>
            Sync[F].delay(
              org.scalajs.dom.window.localStorage.setItem(kernel.name, id.toString),
            )
          case _ => Applicative[F].unit
        }
      session <- Resource.eval(Session.make[F, kernel.S, kernel.U](kernel.initState, None))
      sessionStates: Stream[F, FennecSession[kernel.S, kernel.U]] = allMessages.evalMap {
        case (m @ Message.EventMessage(-1, _), Direction.Outgoing) =>
          for
            counter <- session.incrementCounter
            mUpdated = m.copy(sessionCounter = counter)
            _                      <- outgoingTopic.publish1(fromM(kernel)(mUpdated))
            given Session[F, S, U] <- session.pure
            r     <- userProtocol.handleF(outgoingTopicM.publish1.andThen(_.void))(mUpdated)
            state <- session.get
          yield state
        case (m, Direction.Outgoing) =>
          for
            _                      <- outgoingTopic.publish1(fromM(kernel)(m))
            given Session[F, S, U] <- session.pure
            _     <- userProtocol.handleF(outgoingTopicM.publish1.andThen(_.void))(m)
            state <- session.get
          yield state
        case (m, Direction.Incoming) =>
          given Session[F, S, U] = session
          userProtocol.handleF(outgoingTopicM.publish1.andThen(_.void))(m) *> session.get
      }
      _                  <- Resource.eval(outgoingTopicM.publish1(Message.RequestSession(Some(id))))
      sessionStatesTopic <- Resource.eval(Topic[F, FennecSession[kernel.S, kernel.U]])
      _ <- sessionStates
        .through(sessionStatesTopic.publish)
        .drainAsResource // start pulling the whole system
    yield (eventTopic, Subscription(sessionStatesTopic))

end Websocket
