package fennec.examples

import calico.*
import calico.html.Html
import calico.syntax.*
import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.*
import fs2.dom.*
import org.legogroup.woof.*
object Main extends IOApp.Simple:

  val mkLogger =
    given Printer = NoColorPrinter()
    given Filter  = Filter.everything
    DefaultLogger.makeIo(Output.fromConsole[IO])


  def apps(using Dispatcher[IO], Html[IO], Logger[IO]): List[FennecApp[IO, ?, ?]] = List(
    CounterApp[IO](),
    TodoApp[IO](),
  )

  override def run: IO[Unit] =
    given Html[IO] = calico.html.io
    Dispatcher
      .sequential[IO]
      .use(implicit dispatcher =>
        for
          given Logger[IO] <- mkLogger
          root             <- Window[IO].document.getElementById("app").map(_.get)
          _                <- apps.traverse(app => app.resource.renderInto(root)).useForever
        yield (),
      )

end Main
