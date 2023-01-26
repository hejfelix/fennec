import fennec.examples.TodoKernel.*
import cats.effect.std.UUIDGen
import fennec.UpdateEffect
import cats.syntax.all.*
import cats.Applicative

def newTodo[F[_]: UUIDGen: Applicative]: UpdateEffect[F, State, Event, Unit] =
  _ =>
    state =>
      case Event.CreateTodo =>
        UUIDGen[F].randomUUID
          .map(Id(_))
          .map(id => List(Event.NewTodo(id), Event.UpdateTodo(Todo("TITLE", "MESSAGE", id))))
      case _ => List.empty.pure[F]
