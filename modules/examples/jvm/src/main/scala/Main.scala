import fennec.examples.*
import fennec.server.http4s.KernelService
import fennec.{KernelCatsSupport, Log}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router

import org.legogroup.woof.{*, given}
import org.legogroup.woof.slf4j.registerSlf4j
import KernelCatsSupport.given
import cats.effect.std.Random

object Main extends IOApp:
  given Printer = ColorPrinter()
  given Filter  = Filter.everything

  override def run(args: List[String]): IO[ExitCode] =
    for
      given Logger[IO] <- DefaultLogger.makeIo(Output.fromConsole)
      given Random[IO] <- Random.scalaUtilRandom[IO]
//      _                <- Logger[IO].registerSlf
      fennecleRoutes   <- KernelService(FennecleKernel.kernel.withEffect(fennecleEffects[IO])).route
      counterRoutes    <- KernelService(CounterKernel.kernel.covary[IO]).route
      lifeRoutes       <- KernelService(LifeKernel.kernel.covary[IO]).route
      formRoutes       <- KernelService(FormsKernel.kernel.covary[IO]).route
      todoRoutes       <- KernelService(TodoKernel.kernel.withEffect(newTodo[IO])).route
      _                <- Logger[IO].info(s"Starting...")
      exitCode <- BlazeServerBuilder[IO]
        .bindHttp(8080, "localhost")
        .withHttpWebSocketApp(builder =>
          Router(
            "fennec/counter"  -> counterRoutes(builder),
            "fennec/life"     -> lifeRoutes(builder),
            "fennec/formApp"  -> formRoutes(builder),
            "fennec/fennecle" -> fennecleRoutes(builder),
            "todo"     -> todoRoutes(builder),
          ).orNotFound,
        )
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    yield exitCode

end Main
