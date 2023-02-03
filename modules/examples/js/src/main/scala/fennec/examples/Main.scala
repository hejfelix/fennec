package fennec.examples

import calico.*
import calico.html.Html
import calico.syntax.*
import cats.effect.*
import cats.effect.std.Dispatcher
import fennec.Kernel
import fennec.KernelCatsSupport.given
import fs2.*
import fs2.dom.*
import org.legogroup.woof.{*}
import org.scalajs.dom.window

object Main extends IOApp.Simple:

  import CounterKernel.{Event, State}
  val kernel: Kernel[IO, State, Event, Unit] =
    CounterKernel.kernel.covary[IO]

  val mkLogger =
    given Printer = NoColorPrinter()
    given Filter  = Filter.everything
    DefaultLogger.makeIo(Output.fromConsole[IO])

  val wsUrl = s"ws://${window.location.host}${window.location.pathname}fennec/${kernel.name}"

  println(s"WAAAT")

  override def run: IO[Unit] =
    given Html[IO] = calico.html.io

    Dispatcher
      .sequential[IO]
      .use(implicit dispatcher =>
        for
          given Logger[IO] <- mkLogger
          root             <- Document[IO].getElementById("app").map(_.get)
          _                <- CounterApp[IO]().resource.renderInto(root).useForever
        yield (),
      )

end Main
