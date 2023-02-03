import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.host
import fennec.KernelCatsSupport
import fennec.KernelCatsSupport.given
import fennec.examples.*
import fennec.server.http4s.KernelService
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import org.legogroup.woof.{*, given}

object Main extends IOApp:
  given Printer = ColorPrinter()
  given Filter  = Filter.everything

  override def run(args: List[String]): IO[ExitCode] =
    for
      given Logger[IO] <- DefaultLogger.makeIo(Output.fromConsole)
      given Random[IO] <- Random.scalaUtilRandom[IO]
      fennecleRoutes   <- KernelService(FennecleKernel.kernel.withEffect(fennecleEffects[IO])).route
      counterRoutes    <- KernelService(CounterKernel.kernel.covary[IO]).route
      lifeRoutes       <- KernelService(LifeKernel.kernel.covary[IO]).route
      formRoutes       <- KernelService(FormsKernel.kernel.covary[IO]).route
      todoRoutes       <- KernelService(TodoKernel.kernel.withEffect(newTodo[IO])).route
      _                <- Logger[IO].info(s"Starting...")
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(host"127.0.0.1")
        .withHttpWebSocketApp(builder =>
          Router(
            "fennec/counter"  -> counterRoutes(builder),
            "fennec/life"     -> lifeRoutes(builder),
            "fennec/formApp"  -> formRoutes(builder),
            "fennec/fennecle" -> fennecleRoutes(builder),
            "fennec/todo"     -> todoRoutes(builder),
          ).orNotFound,
        )
        .build
        .useForever
    yield ExitCode.Success

end Main
