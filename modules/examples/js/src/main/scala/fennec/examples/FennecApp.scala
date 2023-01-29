package fennec.examples

import fennec.Kernel
import org.scalajs.dom.window
import java.util.UUID
import cats.effect.kernel.Sync
import scala.util.Try
import cats.Monad
import cats.syntax.all.*
import cats.effect.std.UUIDGen
import fs2.concurrent.Topic
import fs2.Stream
import fs2.dom.HtmlElement
import cats.effect.kernel.Resource
import cats.Functor
import fennec.client.Websocket
import cats.effect.std.Dispatcher
import cats.effect.kernel.Clock
import org.legogroup.woof.Logger
import cats.effect.kernel.Async
import calico.html.Html
import fennec.client.KernelSocket

trait FennecApp[F[_]: Html: Async: Logger: Dispatcher: UUIDGen: LocalStorage, State, Event](
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

  def resource: Resource[F, HtmlElement[F]] =
    for
      id                        <- Resource.eval(getId)
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
