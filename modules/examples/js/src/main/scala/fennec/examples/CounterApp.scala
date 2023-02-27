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

class CounterApp[F[_]: Html: Async: Dispatcher: UUIDGen: Logger: LocalStorage](selfSource: Map[String, String])
    extends FennecApp[F, State, Event](kernel.covary[F], selfSource):

  val html: Html[F] = summon[Html[F]]
  import html.{*, given}

  private val buttonClass =
    cls := "m-2 transition bg-blue-500 hover:bg-green-500 text-white font-bold py-2 px-4 rounded-full hover:scale-150"

  override def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    div(
      p(cls := "text-4xl", states.map(_.count.toString).holdOptionResource),
      br(()),
      button("+", onClick.as(Event.Increment) --> outgoing.publish, buttonClass),
      button("-", onClick.as(Event.Decrement) --> outgoing.publish, buttonClass),
      button(span(cls := "icon", i(cls := "fab fa-twitter"))),
    )

end CounterApp
