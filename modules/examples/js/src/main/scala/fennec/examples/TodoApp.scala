package fennec.examples

import calico.html.Html
import cats.effect.kernel.{Async, Resource}
import cats.effect.std.{Dispatcher, UUIDGen}
import cats.syntax.all.*
import fennec.KernelCatsSupport.given
import fennec.examples.TodoKernel.{Event, State, Todo, kernel}
import fs2.Stream
import fs2.concurrent.{Signal, Topic}
import fs2.dom.{HtmlElement, HtmlUListElement}
import org.legogroup.woof.Logger

class TodoApp[F[_]: Html: Async: Dispatcher: UUIDGen: Logger: LocalStorage](
    selfSource: Map[String, String],
) extends FennecApp[F, State, Event](kernel.covary[F], selfSource):

  val html: Html[F] = summon[Html[F]]
  import html.{*, given}

  private val buttonClass =
    cls := "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded-full"

  override def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]] = div(
    button("new", buttonClass, onClick.as(Event.CreateTodo) --> outgoing.publish),
    br(()),
    states
      .map(_.todos)
      .holdResource(List.empty)
      .flatMap(childrenLis(outgoing)),
  )

  private def childrenLis(outgoing: Topic[F, Event])(
      todos: Signal[F, List[Todo]],
  ): Resource[F, HtmlUListElement[F]] =
    ul(children <-- todos.map(_.map(todoItem(outgoing))))

  private def todoItem(outgoing: Topic[F, Event])(todo: Todo): Resource[F, HtmlElement[F]] =
    li(
      b("Title: "),
      input.withSelf(ref =>
        (
          defaultValue := todo.title,
          onBlur --> (_.evalMap(_ => ref.value.get)
            .map(newTitle => Event.UpdateTodo(todo.copy(title = newTitle)))
            .through(outgoing.publish)),
        ),
      ),
      br(()),
      i(
        "Message: ",
        input.withSelf(ref =>
          (
            defaultValue := todo.message,
            onBlur --> (_.evalMap(_ => ref.value.get)
              .map(newMessage => Event.UpdateTodo(todo.copy(message = newMessage)))
              .through(outgoing.publish)),
          ),
        ),
      ),
    )

end TodoApp
