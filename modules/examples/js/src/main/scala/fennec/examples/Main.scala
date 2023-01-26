package fennec.examples

import calico.*
import calico.html.io.{*, given}
import calico.syntax.*
import calico.unsafe.given
import cats.effect.std.{Dispatcher, UUIDGen}
import cats.effect.syntax.all.*
import cats.effect.*
import cats.syntax.all.*
import cats.{Applicative, Id}
import fennec.Kernel
import fennec.KernelCatsSupport.given
import fennec.client.{Subscription, Websocket}
import fs2.*
import fs2.concurrent.*
import fs2.dom.*
import org.legogroup.woof.{*, given}
import org.scalajs.dom.window

import java.util.UUID
import scala.util.Try

object Main extends IOApp.Simple:

  import CounterKernel.{Event, State}
  val kernel: Kernel[IO, State, Event, Unit] =
    CounterKernel.kernel.covary[IO]

  val mkLogger =
    given Printer = NoColorPrinter()
    given Filter  = Filter.everything
    DefaultLogger.makeIo(Output.fromConsole[IO])

  val wsUrl = s"ws://${window.location.host}${window.location.pathname}fennec/${kernel.name}"
  println(wsUrl)
  private val maybeId: IO[Option[UUID]] = IO.delay {
    val str: String = window.localStorage.getItem(kernel.name)
    if str == null then None else Try(UUID.fromString(str)).toOption
  }

  val getId = for
    idOpt <- maybeId
    id    <- idOpt.fold(UUIDGen[IO].randomUUID)(_.pure[IO])
    _     <- IO(window.localStorage.setItem(kernel.name, id.toString))
  yield id

  def app(using Logger[IO]) =
    for
      given Dispatcher[IO] <- Dispatcher.sequential[IO]
      id                   <- Resource.eval(getId)
      keyboardEvents       <- KeyboardInput.sub[IO]
      _ <- keyboardEvents
        .subscribe(maxQueued = 10)
        .evalTap(event => Logger[IO].debug(s"KEYBOARD EVENT: $event"))
        .compile
        .drain
        .background
      (outgoing, sessionStates) <- Websocket.topicFor(kernel, wsUrl, id)
      userStates = sessionStates.map(_.state)
      _              <- Resource.eval(Logger[IO].info(s"READY"))
      htmlDivElement <- render(outgoing, userStates.t())
    yield htmlDivElement

  def render(outgoing: Topic[IO, Event], states: Stream[IO,State]): Resource[IO, HtmlDivElement[IO]] =
    div(
      i("Count: "),
      b(states.map(_.count.toString).holdOptionResource),
      button(
        "+",
        onClick --> (_.as(Event.Increment).through(outgoing.publish)),
      ),
      button("-", onClick --> (_.as(Event.Decrement).through(outgoing.publish))),
    )

  override def run: IO[Unit] =
    for
      given Logger[IO] <- mkLogger
      root             <- Document[IO].getElementById("app").map(_.get)
      _                <- app.renderInto(root).useForever
    yield ()

end Main
