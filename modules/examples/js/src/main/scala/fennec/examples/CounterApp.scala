package fennec.examples

import CounterKernel.*
import fennec.KernelCatsSupport.given
import cats.Monad
import org.legogroup.woof.Logger
import cats.effect.std.Dispatcher
import cats.effect.std.UUIDGen
import cats.effect.kernel.Async
import calico.html.Html
import fs2.concurrent.Topic
import fs2.Stream
import cats.effect.kernel.Resource
import fs2.dom.HtmlElement

class CounterApp[F[_]: Html: Async: Dispatcher: UUIDGen: Logger: LocalStorage: Monad]
    extends FennecApp[F, State, Event](kernel.covary[F]):

  val html: Html[F] = Html[F]

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
