import cats.Applicative
import fennec.examples.PingKernel.*
import fennec.UpdateEffect
import cats.syntax.all.*

def pingEffect[F[_] : Applicative]: UpdateEffect[F, State, Event, Unit] = _ => state =>
  case Event.Ping if state.count <= iterations => List(Event.Pong).pure[F]
  case _ => List.empty.pure[F]
