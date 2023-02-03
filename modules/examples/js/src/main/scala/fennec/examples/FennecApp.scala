package fennec.examples

import cats.effect.kernel.{Async, Resource}
import cats.effect.std.{Dispatcher, UUIDGen}
import cats.syntax.all.*
import fennec.Kernel
import fennec.client.{KernelSocket, Websocket}
import fs2.Stream
import fs2.concurrent.Topic
import fs2.dom.HtmlElement
import org.legogroup.woof.{Logger, given}
import org.scalajs.dom.window

import java.util.UUID
import scala.annotation.nowarn
import scala.util.Try

@nowarn("msg=unused implicit parameter")
trait FennecApp[F[_]: Async: Logger: Dispatcher: UUIDGen: LocalStorage, State, Event](
    kernel: Kernel[F, State, Event, Unit],
):

  private val wsUrl: String =
    s"ws://${window.location.host}${window.location.pathname}fennec/${kernel.name}"

  private def maybeId: F[Option[UUID]] =
    LocalStorage[F].getItem(kernel.name).map(_.flatMap(str => Try(UUID.fromString(str)).toOption))

  private def getId: F[UUID] = for
    idOpt <- maybeId
    id    <- idOpt.fold(UUIDGen[F].randomUUID)(_.pure[F])
    _     <- LocalStorage[F].setItem(kernel.name, id.toString())
  yield id

  @nowarn("msg=unused pattern variable")
  def resource: Resource[F, HtmlElement[F]] =
    for
      id                        <- Resource.eval(getId)
      _                         <- Resource.eval(Logger[F].info(s"Found $id, using it..."))
      channel                   <- Websocket[F].connectAsChannel(wsUrl)
      (outgoing, sessionStates) <- KernelSocket.topicFor(channel, kernel, id)
      userStates = sessionStates.map(_.state)
      html <- render(outgoing, userStates.t())
    yield html

  def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]]

end FennecApp
