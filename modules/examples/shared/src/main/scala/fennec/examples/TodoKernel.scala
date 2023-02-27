package fennec.examples

import fennec.{Kernel, Update}

import java.util.UUID
import scala.annotation.nowarn
import fennec.CodecDerived.given
import fennec.CodecDefaults.given

object TodoKernel:

  case class Id(id: UUID)
  case class Todo(title: String, message: String, id: Id)
  case class State(todos: List[Todo])

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case CreateTodo
    case NewTodo(id: Id)
    case DeleteTodo(id: Id)
    case UpdateTodo(todo: Todo)

  val initState = State(List(Todo("", "", Id(UUID.randomUUID))))

  @nowarn("msg=unused explicit parameter")
  val update: Update[State, Event] = state =>
    case Event.CreateTodo     => state
    case Event.NewTodo(id)    => state.copy(todos = state.todos.prepended(Todo("", "", id)))
    case Event.DeleteTodo(id) => state.copy(todos = state.todos.filterNot(_.id == id))
    case Event.UpdateTodo(todo) =>
      state.copy(todos = state.todos.map(t => if t.id == todo.id then todo else t))

  val kernel = Kernel.init[State, Event](name = "todo", initState).withUpdate(update)

end TodoKernel
