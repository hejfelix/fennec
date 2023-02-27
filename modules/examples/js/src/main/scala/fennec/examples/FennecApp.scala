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
import calico.html.Html
import java.util.UUID
import scala.annotation.nowarn
import scala.util.Try
import org.scalajs.dom.window

@nowarn("msg=unused implicit parameter")
trait FennecApp[F[_]: Async: Logger: Dispatcher: UUIDGen: LocalStorage, State, Event](
    val kernel: Kernel[F, State, Event, Unit],
    selfSource: Map[String, String],
)(using html: Html[F]):

  private val wsUrl: String =
    if window.location.host.equalsIgnoreCase("hejfelix.github.io") then
      s"wss://fennec.fly.dev/fennec/${kernel.name}"
    else s"ws://${window.location.host}${window.location.pathname}fennec/${kernel.name}"

  private def maybeId: F[Option[UUID]] =
    LocalStorage[F].getItem(kernel.name).map(_.flatMap(str => Try(UUID.fromString(str)).toOption))

  private def getId: F[UUID] = for
    idOpt <- maybeId
    id    <- idOpt.fold(UUIDGen[F].randomUUID)(_.pure[F])
    _     <- LocalStorage[F].setItem(kernel.name, id.toString())
  yield id

  @nowarn("msg=unused pattern variable")
  def resource: Resource[F, HtmlElement[F]] =
    (for
      id                        <- Resource.eval(getId)
      _                         <- Resource.eval(Logger[F].info(s"Found $id, using it...$wsUrl"))
      channel                   <- Websocket[F].connectAsChannel(wsUrl)
      (outgoing, sessionStates) <- KernelSocket.topicFor(channel, kernel, id)
      userStates = sessionStates.map(_.state)
      html <- renderWithSourceCode(outgoing, userStates.t())
      _ = println(html)
    yield html).onFinalize(Logger[F].info(s"GOODBYE ${kernel.name}"))

  def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]]

  def sourceCodeDetails: List[Resource[F, HtmlElement[F]]] =
    import html.{*, given}
    selfSource.toList.map:
      (key,codeString) =>
        detailsTag(
          summaryTag(
            "Src: ", b(key),
            onClick --> (_.foreach(_ =>
              Async[F].delay(scalajs.js.Dynamic.global.hljs.highlightAll()),
            )),
          ),
          pre(code(cls := "language-scala", codeString)),
        )


  def renderWithSourceCode(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    import html.{*, given}
    div(
      cls := "m-8",
      sourceCodeDetails,
      br(()),
      render(outgoing, states),
    )

end FennecApp
