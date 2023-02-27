package fennec.client

import cats.effect.Async
import cats.effect.kernel.*
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fennec.*
import fennec.client.Websocket.*
import fs2.concurrent.Topic
import org.scalajs.dom
import org.scalajs.dom.{MessageEvent, WebSocket}

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

trait Websocket[F[_]]:
  def connect(webSocketUrl: String): Resource[F, dom.WebSocket]
  def connectAsChannel(
      webSocketUrl: String,
  ): Resource[F, WebsocketChannel[F]]
end Websocket

object Websocket:

  case class WebsocketChannel[F[_]](in: Topic[F, MessageEvent], out: Topic[F, MessageEvent])

  def apply[F[_]: Async]: Websocket[F] = new:
    private def create(webSocketUrl: String): F[dom.WebSocket] =
      Async[F].async[dom.WebSocket] { complete =>
        Sync[F].delay {
          val websocket = new dom.WebSocket(webSocketUrl)

          websocket.onopen = _ => complete(websocket.asRight)
          websocket.onerror =
            error => {
              complete(Exception(s"websocket error:$error $webSocketUrl").asLeft)
            }
          websocket.onclose = reason => complete(Exception(s"websocket closed: $reason").asLeft)

          Some(Sync[F].delay(websocket.close()))
        }
      }
    def connect(webSocketUrl: String): Resource[F, dom.WebSocket] =
      Resource.make(create(webSocketUrl))(socket =>
        Sync[F].delay {
          if socket.readyState != 3 /*closed*/ then socket.close()
        },
      )
    override def connectAsChannel(webSocketUrl: String): Resource[F, WebsocketChannel[F]] =
      for
        given Dispatcher[F] <- Dispatcher.sequential
        socket              <- connect(webSocketUrl)
        in                  <- incoming(socket)
        out                 <- outgoing(socket)
      yield WebsocketChannel(in, out)

  def incoming[F[_]: Async](
      websocket: WebSocket,
  )(using dispatcher: Dispatcher[F]): Resource[F, Topic[F, MessageEvent]] =
    for
      incomingTopic <- Resource.make(Topic[F, MessageEvent])(socket =>
        socket.close.flatMap(result => Sync[F].delay(println(result))),
      )
      _ <- Resource.make(
        Sync[F].delay(websocket.onmessage =
          m =>
            dispatcher.unsafeRunAndForget {
              incomingTopic.publish1(m)
            },
        ),
      )(_ => Sync[F].delay(websocket.onmessage = null))
    yield incomingTopic

  /** A topic which immediately consumes its messages by sending them through a websocket
    */
  @nowarn("msg=unused pattern variable")
  def outgoing[F[_]: Async](websocket: WebSocket): Resource[F, Topic[F, MessageEvent]] =
    for
      outgoingTopic  <- Resource.make(Topic[F, MessageEvent])(_.close.void)
      outgoingStream <- outgoingTopic.subscribeAwait(10)
      outgoingWithSend = outgoingStream.evalMap { m =>
        Sync[F].delay(websocket.send(m.data.asInstanceOf[ArrayBuffer]))
      }
      _ <- Resource.make {
        outgoingWithSend.compile.drain.start
      }(_.cancel)
    yield outgoingTopic

end Websocket
