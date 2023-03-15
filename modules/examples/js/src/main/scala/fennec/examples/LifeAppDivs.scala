package fennec.examples

import calico.html.Html
import cats.effect.kernel.{Async, Resource, Sync}
import cats.effect.std.{Dispatcher, UUIDGen}
import fennec.examples.LifeKernel.{Event, State, kernel}
import org.legogroup.woof.Logger
import fennec.KernelCatsSupport.given
import fennec.examples.LifeAppDivs.gridSize
import fs2.concurrent.Topic
import fs2.dom.{HtmlDivElement, HtmlElement}
import cats.syntax.all.*
import fs2.Stream
import scala.concurrent.duration.*

import scala.annotation.nowarn
import cats.effect.syntax.all.*

@nowarn("msg=unused import")
class LifeAppDivs[F[_]: Async: Dispatcher: UUIDGen: Logger: LocalStorage](
    selfSource: Map[String, String],
)(using html: Html[F])
    extends FennecApp[F, State, Event](kernel.covary[F], selfSource):

  import html.{*, given}

  override def render(
      outgoing: Topic[F, Event],
      states: fs2.Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    for
      _       <- events.through(outgoing.publish).compile.drain.background
      element <- doRender(outgoing, states)
    yield element

  def events: Stream[F, Event] = Stream.repeatEval(Sync[F].sleep(1.seconds)).as(Event.Step)

  def doRender(
      outgoing: Topic[F, Event],
      states: fs2.Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    div(
      cls := s"grid place-items-center gap-0 grid-cols-[repeat($gridSize,16px)]",
      cells(states, outgoing),
    )

  def cells(
      states: fs2.Stream[F, State],
      outgoing: Topic[F, Event],
  ): List[Resource[F, HtmlDivElement[F]]] =
    (0 until (gridSize * gridSize)).toList.map(i =>
      cell(i % gridSize, i / gridSize, states, outgoing),
    )

  def cell(
      x: Int,
      y: Int,
      states: fs2.Stream[F, State],
      outgoing: Topic[F, Event],
  ): Resource[F, HtmlDivElement[F]] = div(
    cls <-- states
      .map(s =>
        if s.grid.cells.contains((x, y)) then "bg-black"
        else "bg-white",
      )
      .changes
      .map(color => s"grid-item h-4 border-solid border-gray-200 border-2 w-4 content-center $color")
      .map(List(_))
      .holdResource(List.empty),
    onClick.as(Event.Click(x, y)) --> outgoing.publish,
  )

end LifeAppDivs

object LifeAppDivs:

  val gridSize = 20

end LifeAppDivs
