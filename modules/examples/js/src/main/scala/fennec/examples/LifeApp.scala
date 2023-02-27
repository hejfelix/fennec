package fennec.examples

import calico.html.Html
import cats.effect.Ref
import cats.effect.kernel.{Async, Resource, Sync}
import cats.effect.std.{Dispatcher, UUIDGen}
import fennec.examples.LifeKernel.{Event, State, kernel}
import org.legogroup.woof.Logger
import fennec.KernelCatsSupport.given
import fennec.UpdateEffect
import fs2.concurrent.Topic
import fs2.dom.{HtmlCanvasElement, HtmlElement}
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, MouseEvent}
import org.scalajs.dom.html.Canvas
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fennec.examples.LifeApp.Frequency
import fs2.Stream

import concurrent.duration.*
import scala.annotation.nowarn

@nowarn
class LifeApp[F[_]: Async: Dispatcher: UUIDGen: Logger: LocalStorage](
    frequency: Ref[F, Frequency],
    selfSource: Map[String, String],
)(using html: Html[F])
    extends FennecApp[F, State, Event](
      kernel.withEffect(LifeApp.updateEffect(frequency)),
      selfSource,
    ):

  import html.{*, given}

  case class Vec2D(x: Double, y: Double)

  def lifeCanvas(outgoing: Topic[F, Event]): Resource[F, HtmlCanvasElement[F]] =
    canvasTag.withSelf((self: HtmlCanvasElement[F]) =>
      (
        idAttr     := LifeApp.canvasId,
        widthAttr  := 700,
        heightAttr := 700,
        onClick --> (_.map(mouseEventToPoint(self, _)).through(outgoing.publish)),
      ),
    )

  def mouseEventToPoint(canvas: HtmlCanvasElement[F], event: fs2.dom.MouseEvent[F]) =
    import reflect.Selectable.reflectiveSelectable
    val rect = canvas
      .asInstanceOf[dom.HTMLCanvasElement] // close your eyes
      .getBoundingClientRect()
    val domEvent =
      event
        .asInstanceOf[{ val event: dom.MouseEvent }]
        .event // until it's implemented in fs2.dom
    Event.Click(
      x = (domEvent.clientX - rect.left).toInt / LifeApp.squareWidth,
      y = (domEvent.clientY - rect.top).toInt / LifeApp.squareWidth,
    )

  def events: Stream[F, Event] = fs2.Stream.repeatEval(
    for
      freq <- frequency.get
      _    <- Sync[F].sleep(freq.toDuration)
    yield Event.Step,
  )

  override def render(
      outgoing: Topic[F, Event],
      states: fs2.Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    for
      _    <- events.through(outgoing.publish).compile.drain.background
      html <- p(cls := "text-4xl", "LIFE APP", lifeCanvas(outgoing))
    yield html

end LifeApp

object LifeApp:

  val canvasId    = "life-canvas"
  val squareWidth = 15

  def context[F[_]: Sync]: F[CanvasRenderingContext2D] = Sync[F].delay {
    val canvas = dom.document.querySelector(s"#$canvasId").asInstanceOf[Canvas]
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  }
  def clear[F[_]: Sync](x: Int, y: Int)(using ctx: CanvasRenderingContext2D): F[Unit] =
    Sync[F].delay(ctx.clearRect(x * squareWidth, y * squareWidth, squareWidth, squareWidth))

  def fill[F[_]: Sync](x: Int, y: Int)(using ctx: CanvasRenderingContext2D): F[Unit] =
    Sync[F].delay(
      ctx.fillRect(x * squareWidth, y * squareWidth, squareWidth, squareWidth),
    )

  opaque type Frequency = FiniteDuration
  object Frequency:
    def apply(f: FiniteDuration): Frequency = f
  extension (f: Frequency) def toDuration: FiniteDuration = f

  def updateEffect[F[_]: Sync](frequency: Ref[F, Frequency]): UpdateEffect[F, State, Event, Unit] =
    _ =>
      state =>
        case Event.ChangeFrequency(f) => frequency.set(f.milliseconds).as(List.empty)
        case Event.NoOp               => List.empty.pure[F]

        case Event.Step =>
          val before      = state.grid.cells
          val after       = state.grid.step.cells
          val toKill      = before diff after
          val toResurrect = after diff before

          for
            given CanvasRenderingContext2D <- context[F]
            _                              <- toKill.toList.traverse(clear)
            _                              <- toResurrect.toList.traverse(fill)
          yield List.empty

        case Event.Click(x, y) =>
          for
            given CanvasRenderingContext2D <- context[F]
            _                              <- fill(x, y)
          yield List.empty

        case Event.Clear =>
          for
            given CanvasRenderingContext2D <- context[F]
            _                              <- state.grid.cells.toList.traverse(clear)
          yield List.empty

        case Event.Initialize =>
          for
            _                              <- frequency.set(state.updateFrequency.milliseconds)
            given CanvasRenderingContext2D <- context[F]
            _                              <- state.grid.cells.toList.traverse(fill)
          yield List.empty
end LifeApp
