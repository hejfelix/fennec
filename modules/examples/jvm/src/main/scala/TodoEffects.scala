import fennec.examples.TodoKernel.*
import cats.effect.std.UUIDGen
import fennec.UpdateEffect
import cats.syntax.all.*
import cats.Applicative

def newTodo[F[_]: UUIDGen: Applicative]: UpdateEffect[F, State, Event, Unit] =
  _ =>
    _ =>
      case Event.CreateTodo =>
        UUIDGen[F].randomUUID
          .map(Id.apply)
          .map(id => List(Event.NewTodo(id), Event.UpdateTodo(Todo("", "", id))))
      case _ => List.empty.pure[F]
