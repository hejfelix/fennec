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

import java.util.UUID
import scala.concurrent.duration.*
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
    ul(
      children[UUID](id =>
        for
          todosPure <- Resource.eval(todos.get)
          todo      <- Resource.pure(todosPure.find(_.id.id == id).get)
          html      <- todoItem(outgoing)(todo)
        yield html,
      ) <-- todos.map(_.map(_.id.id)),
    )

  private def todoItem(outgoing: Topic[F, Event])(todo: Todo): Resource[F, HtmlElement[F]] =
    val inputStyle =
      cls := "shadow appearance-none border border-red-500 rounded py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline"
    li(
      cls <-- (Stream("translate-x-full opacity-0".split(" ").toList)
        ++ Stream
          .awakeDelay(30.milliseconds)
          .as("transition ease-in-out duration-1000 -translate-x-0 opacity-100".split(" ").toList))
        .holdResource(List.empty),
      label("Title", cls := "block text-gray-700 text-sm font-bold mb-2"),
      input.withSelf { ref =>
        (
          defaultValue := todo.title,
          inputStyle,
          placeholder := "Enter title here",
          onBlur --> (_.evalMap(_ => ref.value.get)
            .map(newTitle => Event.UpdateTodo(todo.copy(title = newTitle)))
            .through(outgoing.publish)),
        )
      },
      br(()),
      label("Message", cls := "block text-gray-700 text-sm italic mb-2"),
      input.withSelf(ref =>
        (
          defaultValue := todo.message,
          inputStyle,
          placeholder := "Enter description here",
          onBlur --> (
            _.evalMap(_ => ref.value.get)
              .map(newMessage => Event.UpdateTodo(todo.copy(message = newMessage)))
              .through(outgoing.publish),
          ),
        ),
      ),
      br(()),
      button(
        "Delete",
        cls := "bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline",
        onClick.as(Event.DeleteTodo(todo.id)) --> outgoing.publish,
      ),
    )

end TodoApp
