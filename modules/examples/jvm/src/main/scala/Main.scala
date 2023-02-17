import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{host, port}
import fennec.{Kernel, KernelCatsSupport}
import fennec.KernelCatsSupport.given
import fennec.examples.*
import fennec.server.http4s.KernelService
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder2
import org.legogroup.woof.{*, given}
import cats.syntax.all.*
import org.http4s.server.middleware.{CORS, CORSPolicy}

object Main extends IOApp:
  given Printer = ColorPrinter()
  given Filter  = Filter.everything

  def kernels(using Random[IO]): List[Kernel[IO, ?,?,?]] = List(
    FennecleKernel.kernel.withEffect(fennecleEffects[IO]),
    CounterKernel.kernel.covary[IO],
    LifeKernel.kernel.covary[IO],
    FormsKernel.kernel.covary[IO],
    TodoKernel.kernel.withEffect(newTodo[IO])
  )

  def routes(using Random[IO], Logger[IO]): IO[List[(String, WebSocketBuilder2[IO] => HttpRoutes[IO])]] = kernels.traverse(kernel => KernelService(kernel).route.fproductLeft(_ => s"fennec/${kernel.name}"))

  val corsConfig: CORSPolicy =
    CORS.policy.withAllowOriginAll.withAllowMethodsAll

  override def run(args: List[String]): IO[ExitCode] =
    for
      given Logger[IO] <- DefaultLogger.makeIo(Output.fromConsole)
      given Random[IO] <- Random.scalaUtilRandom[IO]
      _                <- Logger[IO].info(s"Starting...")
      routePairs <- routes
      server <- EmberServerBuilder
        .default[IO]
        .withHost(host"0.0.0.0")
        .withPort(port"8080")
        .withHttpWebSocketApp(webSocketBuilder =>
          corsConfig(Router(
            routePairs.map((path,routeBuilder) => path -> routeBuilder(webSocketBuilder))*
          ).orNotFound)
        )
        .build.pure[IO]
      _ <- server.use:
        server =>
          for
            _ <- Logger[IO].info(s"Server is ready:")
            _ <- routePairs.traverse( (path,_) =>Logger[IO].info(s"http:/${server.address}/$path"))
            _ <- IO.never
          yield ()
    yield ExitCode.Success

end Main
