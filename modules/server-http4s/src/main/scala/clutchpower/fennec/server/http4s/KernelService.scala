package fennec.server.http4s

import cats.effect.kernel.Clock
import cats.effect.mtl.*
import cats.effect.std.{Console, UUIDGen}
import cats.effect.syntax.all.*
import cats.effect.{Concurrent, Ref, Spawn}
import cats.mtl.Stateful
import cats.syntax.all.*
import fennec.server.ServerProtocol
import fennec.{Direction, Kernel, Message, UserProtocol}
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Response}
import org.legogroup.woof.Logger.withLogContext
import org.legogroup.woof.{Logger, given}
import scodec.bits.ByteVector

import scala.annotation.nowarn

@nowarn("msg=unused implicit parameter")
class KernelService[F[_]: Clock: Console: Logger: Spawn: UUIDGen: Concurrent, State, Event, User](
    kernel: Kernel[F, State, Event, User],
):

  val dsl = new Http4sDsl[F] {}
  import dsl.*

  type M = Message[State, Event, User]

  def route: F[WebSocketBuilder2[F] => HttpRoutes[F]] =
    for
      given Stateful[F, ServerProtocol.StateMap[State, User]] <- Ref[F]
        .of(ServerProtocol.StateMap.empty[State, User])
        .stateInstance
    yield (websocketBuilder: WebSocketBuilder2[F]) =>
      HttpRoutes.of[F] { case GET -> Root =>
        microAppSocket(websocketBuilder)
      }

  private def enqueueing(
      t: Topic[F, (WebSocketFrame, Direction)],
      direction: Direction,
  ): Pipe[F, WebSocketFrame, Unit] =
    _.evalMap(frame => t.publish1((frame, direction)).void)

  def emitting = (t: Topic[F, (WebSocketFrame, Direction)]) =>
    (m: M, dir: Direction) =>
      val bytes = kernel.messageCodec.encode(m)
      val frame = WebSocketFrame.Binary(ByteVector(bytes.toArray), true)
      t.publish1((frame, dir)).void

  val logAll: Pipe[F, (WebSocketFrame, Direction), (WebSocketFrame, Direction)] = s =>
    s.evalTap((m, dir) =>
      val dirstr  = if dir == Direction.Outgoing then ">>>" else "<<<"
      val decoded = kernel.messageCodec.decode.run(m.data.toArray.toVector)
      val mm      = decoded
      Logger[F].info(s"$dirstr ${mm.map(_.toString).getOrElse("")}  ").whenA(decoded.isRight),
    )

  private def response(
      topic: Topic[F, (WebSocketFrame, Direction)],
      websocketBuilder: WebSocketBuilder2[F],
  )(using
      S: Stateful[F, ServerProtocol.StateMap[State, User]],
  ): F[Response[F]] =

    val emit         = emitting(topic)
    val userProtocol = new UserProtocol(kernel)
    val protocol =
      new ServerProtocol(
        kernel,
        userProtocol,
        m => emit(m, Direction.Outgoing),
      )
    val outgoing = topic.subscribe(10).through(logAll).collect { case (m, Direction.Outgoing) => m }
    val incoming = topic.subscribe(10).through(logAll)

    for
      response <- websocketBuilder.build(
        send = outgoing,
        receive = enqueueing(topic, Direction.Incoming),
      )
      protocol <- protocol.protocol
      _        <- protocol(incoming.through(parse)).compile.drain.start
    yield response
  end response

  private def microAppSocket(websocketBuilder: WebSocketBuilder2[F])(using
      S: Stateful[F, ServerProtocol.StateMap[State, User]],
  ): F[Response[F]] =
    for
      topic <- Topic[F, (WebSocketFrame, Direction)]
      resp  <- response(topic, websocketBuilder).withLogContext("kernel", kernel.name)
    yield resp

  def parse: Pipe[F, (WebSocketFrame, Direction), (kernel.M, Direction)] =
    _.map {
      case (WebSocketFrame.Binary(data, _), dir) =>
        val message: Either[Throwable, (Vector[Byte], M)] =
          kernel.messageCodec.decode.run(data.toArray.toVector)
        val (_, decoded: kernel.M) =
          message.getOrElse(sys.error(s"Failed to parse: $data  $message"))
        Stream.emit((decoded, dir))
      case (WebSocketFrame.Close(status), _) =>
        Stream.exec(
          Logger[F].error(s"Socket closed prematurely, ending stream with status: $status"),
        )
      case (m, _) =>
        Stream.exec(Logger[F].error(s"Socket closed prematurely, unexpected message: $m"))
    }.flatten

end KernelService
