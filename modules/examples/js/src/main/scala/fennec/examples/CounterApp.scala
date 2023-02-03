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

class CounterApp[F[_]: Html: Async: Dispatcher: UUIDGen: Logger: LocalStorage]
    extends FennecApp[F, State, Event](kernel.covary[F]):

  val html: Html[F] = summon[Html[F]]

  def render(
      outgoing: Topic[F, Event],
      states: Stream[F, State],
  ): Resource[F, HtmlElement[F]] =
    import html.{*, given}
    div(
      b(states.map(_.count.toString).holdOptionResource),
      button("+", onClick --> (_.as(Event.Increment).through(outgoing.publish))),
      button("-", onClick --> (_.as(Event.Decrement).through(outgoing.publish))),
    )

end CounterApp
