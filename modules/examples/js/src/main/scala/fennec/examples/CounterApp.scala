package fennec.examples

import calico.html.Html
import cats.effect.kernel.{Async, Resource}
import cats.effect.std.{Dispatcher, UUIDGen}
import fennec.KernelCatsSupport.given
import fennec.examples.CounterKernel.*
import fs2.Stream
import fs2.concurrent.Topic
import org.legogroup.woof.Logger
import fs2.dom.HtmlElement
import cats.syntax.all.*

class CounterApp[F[_]: Html: Async: Dispatcher: UUIDGen: Logger: LocalStorage]
    extends FennecApp[F, State, Event](kernel.covary[F]):

  import html.{*, given}

  val html: Html[F] = summon[Html[F]]

  private val buttonClass =
    cls := "button is-primary is-outlined"

  override def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    div(
      b(states.map(_.count.toString).holdOptionResource),
      button("+", onClick.as(Event.Increment) --> outgoing.publish, buttonClass),
      button("-", onClick.as(Event.Decrement) --> outgoing.publish, buttonClass),
      button(span(cls := "icon", i(cls := "fab fa-twitter"))),
    )

end CounterApp
