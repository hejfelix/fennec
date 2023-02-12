package fennec.examples

import calico.*
import calico.html.Html
import calico.syntax.*
import cats.effect.{IO, *}
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import org.legogroup.woof.*

object Main extends IOApp.Simple:

  given Html[IO] = calico.html.io
  import calico.html.io.{*, given}

  val mkLogger =
    given Printer = NoColorPrinter()
    given Filter  = Filter.everything
    DefaultLogger.makeIo(Output.fromConsole[IO])

  def apps(using Dispatcher[IO], Html[IO], Logger[IO]): List[FennecApp[IO, ?, ?]] = List(
    CounterApp[IO](),
    TodoApp[IO](),
  )

  def currentApp(using Dispatcher[IO], Html[IO], Logger[IO]): IO[SignallingRef[IO, String]] =
    SignallingRef[IO].of(
      apps.map(_.kernel.name).headOption.orEmpty,
    )

  override def run: IO[Unit] =
    Dispatcher
      .sequential[IO]
      .use(implicit dispatcher =>
        for
          given Logger[IO] <- mkLogger
          currentAppRef    <- currentApp
          root             <- Window[IO].document.getElementById("app").map(_.get)
          _ <- exampleApp(currentAppRef)
            .onFinalize(IO.println("BYE"))
            .renderInto(root)
            .useForever
        yield (),
      )

  def exampleApp(using Dispatcher[IO], Html[IO], Logger[IO])(
      currentApp: SignallingRef[IO, String],
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      navigation(currentApp),
      children[String](name => appByName(name)) <-- currentApp.changes.map(List(_)),
//      children[String](name => div("THIS IS THE CURRENT: ", name)) <-- currentApp.changes.map(
//        List(_),
//      ),
//      currentApp.changes.map(name => apps.find(_.kernel.name == name).map(_.resource)),
    )

  def appByName(
      name: String,
  )(using Dispatcher[IO], Html[IO], Logger[IO]): Resource[IO, HtmlElement[IO]] =
    apps.find(_.kernel.name == name).get.resource

  def navigation(
      currentApp: SignallingRef[IO, String],
  )(using Dispatcher[IO], Html[IO], Logger[IO]) =
    div(
      navTag(
        cls  := "navbar",
        role := List("navigation"),
        div(
          cls := "navbar-menu",
          div(
            cls := "navbar-start",
            apps
              .map(_.kernel.name)
              .map(kernelName =>
                a(
                  cls := "navbar-item",
                  kernelName.capitalize,
                  onClick --> (_.foreach(_ => currentApp.set(kernelName))),
                ),
              ),
          ),
        ),
      ),
      br(""),
    )

end Main
