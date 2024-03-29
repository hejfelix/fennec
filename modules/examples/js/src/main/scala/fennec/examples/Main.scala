package fennec.examples

import calico.*
import calico.html.Html
import calico.syntax.*
import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fennec.examples.LifeApp.Frequency
import fennec.examples.macros.SourceFileMacro
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import org.legogroup.woof.*

import scala.annotation.nowarn
import scala.concurrent.duration.*

object Main extends IOApp.Simple:

  given Html[IO] = calico.html.io

  import calico.html.io.{*, given}

  val mkLogger =
    given Printer = NoColorPrinter()

    given Filter = Filter.everything

    DefaultLogger.makeIo(Output.fromConsole[IO])

  @nowarn
  def apps(
            frequency: Ref[IO, Frequency],
          )(using Dispatcher[IO], Html[IO], Logger[IO]): List[FennecApp[IO, ?, ?]] =
    List(
      CounterApp[IO](
        Map(
          SourceFileMacro.getContent[CounterApp[?]],
          SourceFileMacro.getContent[fennec.examples.CounterKernel.type],
        ),
      ),
      TodoApp[IO](
        Map(
          SourceFileMacro.getContent[TodoApp[?]],
          SourceFileMacro.getContent[fennec.examples.TodoKernel.type],
        ),
      ),
      //      LifeApp[IO](
      //        frequency,
      //        Map(SourceFileMacro.getContent[LifeApp[?]], SourceFileMacro.getContent[LifeKernel.type]),
      //      ),
      LifeAppDivs[IO](
        Map(SourceFileMacro.getContent[LifeAppDivs[?]], SourceFileMacro.getContent[LifeKernel.type]),
      ),
      PingApp[IO](
        Map(SourceFileMacro.getContent[PingApp[?]], SourceFileMacro.getContent[PingKernel.type])
      )
    )

  def currentApp(
                  frequency: Ref[IO, Frequency],
                )(using Dispatcher[IO], Html[IO], Logger[IO]): IO[SignallingRef[IO, String]] =
    SignallingRef[IO].of(
      apps(frequency).map(_.kernel.name).headOption.orEmpty,
    )

  override def run: IO[Unit] =
    Dispatcher
      .sequential[IO]
      .use(implicit dispatcher =>
        for
          given Logger[IO] <- mkLogger
          frequency <- Ref[IO].of(LifeApp.Frequency(1.seconds))
          currentAppRef <- currentApp(frequency)
          root <- Window[IO].document.getElementById("app").map(_.get)
          _ <- exampleApp(currentAppRef, frequency)
            .onFinalize(IO.println("BYE"))
            .renderInto(root)
            .useForever
        yield (),
      )

  def exampleApp(using Dispatcher[IO], Html[IO], Logger[IO])(
    currentApp: SignallingRef[IO, String],
    frequency: Ref[IO, Frequency],
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      navigation(currentApp, frequency),
      children[String](name => appByName(frequency)(name)) <-- currentApp.changes.map(List(_)),
    )

  def appByName(frequency: Ref[IO, Frequency])(
    name: String,
  )(using Dispatcher[IO], Html[IO], Logger[IO]): Resource[IO, HtmlElement[IO]] =
    apps(frequency).find(_.kernel.name == name).get.resource

  def navigation(
                  currentApp: SignallingRef[IO, String],
                  frequency: Ref[IO, Frequency],
                )(using Dispatcher[IO], Html[IO], Logger[IO]): Resource[IO, HtmlElement[IO]] =
    val selectedStyle =
      "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500"
    val notSelectedStyle =
      "inline-block p-4 rounded-t-lg hover:text-gray-600 hover:bg-gray-50 dark:hover:bg-gray-800 dark:hover:text-gray-300"
    div(
      ul(
        cls := "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400",
        apps(frequency)
          .map(_.kernel.name)
          .map(kernelName =>
            li(
              cls := "mr-2",
              a(
                cls <-- currentApp
                  .map(selectedApp =>
                    if kernelName.equalsIgnoreCase(selectedApp) then selectedStyle
                    else notSelectedStyle,
                  )
                  .map(_.split(" ").toList),
                kernelName.capitalize,
                onClick --> (_.foreach(_ => currentApp.set(kernelName))),
              ),
            ),
          ),
      ),
      br(""),
    )

end Main
