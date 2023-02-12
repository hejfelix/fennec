package fennec.server

import cats.effect.kernel.{Clock, Ref}
import cats.effect.mtl.*
import cats.effect.std.UUIDGen
import cats.effect.IO
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.{Applicative, Id, ~>}
import fennec.*
import fennec.Codec.DecoderT
import fennec.Message.*
import fennec.server.Utils.*
import fs2.Stream
import org.legogroup.woof.*
import weaver.*

import java.util.UUID
import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

object ServerProtocolSuite extends SimpleIOSuite:

  case class State(count: Int)

  given Printer = ColorPrinter()
  given Filter  = Filter.nothing
  given Logger[IO] =
    DefaultLogger.makeIo(Output.fromConsole).unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

  @nowarn // compiler bug AFAIK
  enum Event:
    case Increment
    case Decrement

  given Codec[Int] = CodecDefaults.given_Codec_Int

  given Codec[State] = CustomCodecSupport.codecVia[State, Int](_.count, State(_))

  type User = Unit

  given Codec[Event] with
    def encode(e: Event): Vector[Byte] = e match
      case Event.Increment => Vector(0x00: Byte)
      case Event.Decrement => Vector(0x01: Byte)

    def decode: DecoderT[Event] =
      Codec.DecoderT.byte.map {
        case 0x00 => Event.Increment
        case 0x01 => Event.Decrement
        case _    => ???
      }
  end given

  given Codec[Unit] with
    @nowarn
    def encode(u: Unit): Vector[Byte] = Vector.empty
    def decode: DecoderT[Unit]        = DecoderT.const(())

  val update: Update[State, Event] =
    s =>
      case Event.Increment => State(s.count + 1)
      case Event.Decrement => State(s.count - 1)

  given (Id ~> IO) = new (Id ~> IO):
    def apply[A](a: A) = IO.pure(a)

  val kernel: Kernel[IO, State, Event, Unit] =
    Kernel
      .init[State, Event]("counter", State(0))
      .withUpdate(update)
      .withEffect(_ =>
        _ =>
          {
            case Event.Decrement =>
              List(
                Event.Increment,
                Event.Increment,
                Event.Increment,
              ).pure[IO]

            case Event.Increment => List.empty.pure
          },
      )

  def ignore[T]: T => IO[Unit] = _ => IO.unit

  test("scanReset") {

    val s = Stream(1, 2, 3, 4, 1, 2, 3, 3, 1, 2, 2).covary[IO]
    val grouped: Stream[IO, Int] = s.scanReset(0, (a, b) => (a + b).pure[IO]) { case 1 =>
      0.pure[IO]
    }

    grouped.compile.toList.map(actual => expect(actual == List(1, 3, 6, 10, 1, 3, 6, 9, 1, 3, 5)))
  }

  test("handleSession") {

    val messages: Stream[IO, (kernel.M, Direction)] = Stream
      .emits(
        Seq(
          EventMessage(0, Event.Increment),
          EventMessage(1, Event.Increment),
          EventMessage(2, Event.Increment),
          EventMessage(3, Event.Decrement),
        ),
      )
      .covary[IO]
      .fproduct(_ => Direction.Outgoing)

    val session = KernelSession.AnonymousSession(UUID.randomUUID())
    given UUIDGen[IO] with
      def randomUUID = IO.pure(session.id)

    val userProtocol = new UserProtocol(kernel)

    given Clock[IO] = new Clock[IO]:
      def monotonic: IO[FiniteDuration]            = FiniteDuration.apply(0, "millis").pure
      def realTime: IO[FiniteDuration]             = monotonic
      def applicative: Applicative[cats.effect.IO] = Applicative[IO]

    val protocol = new ServerProtocol(kernel, userProtocol, ignore)

    val expected: List[FennecSession[State, kernel.U]] = List(
      FennecSession(session, State(1), 1, 0),
      FennecSession(session, State(2), 2, 0),
      FennecSession(session, State(3), 3, 0),
      FennecSession(session, State(2), 4, 0),
    )

    for
      given Stateful[IO, ServerProtocol.StateMap[State, User]] <- Ref[IO]
        .of(ServerProtocol.StateMap.empty[State, User])
        .stateInstance
      given Session[IO, State, User] <- Session.make[IO, State, User](kernel.initState, None)
      res                            <- protocol.handleMessages(messages).compile.toList
    yield expect(res == expected)
  }

  test("effects generate messages") {

    given Clock[IO] = new Clock[IO]:
      def monotonic: IO[FiniteDuration]            = FiniteDuration.apply(0, "millis").pure
      def realTime: IO[FiniteDuration]             = monotonic
      def applicative: Applicative[cats.effect.IO] = Applicative[IO]


    val session       = KernelSession.AnonymousSession(UUID.randomUUID())
    given UUIDGen[IO] with
      def randomUUID = IO.pure(session.id)

    // IDGAF
    var emitted: List[kernel.M] = Nil

    val emit         = (m: kernel.M) => IO.delay { emitted = m :: emitted }
    val userProtocol = new UserProtocol(kernel)

    val generatedMessages = (0 to 2).toList.map(i => Message.EventMessage(5 + i, Event.Increment))

    for
//      given Stateful[IO, ServerProtocol.StateMap[State, User]] <- Ref[IO]
//        .of(ServerProtocol.StateMap.empty[State, User])
//        .stateInstance
      given Session[IO, State, User] <- Session.make[IO, State, User](kernel.initState, None)
      _ <- List
        .fill(4)(-1)
        .traverse_(_ => summon[Session[IO, State, User]].incrementCounter,
        ) // fast forward session to start at 5
      _ <- userProtocol.handleF(emit)(EventMessage(5, Event.Decrement))
    yield expect(emitted.reverse == generatedMessages)
  }
end ServerProtocolSuite
