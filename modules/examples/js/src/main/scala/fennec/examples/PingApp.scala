package fennec.examples

import cats.syntax.all.*
import calico.html.Html
import cats.Applicative
import cats.effect.Clock
import cats.effect.kernel.{Async, Resource}
import cats.effect.std.{Dispatcher, UUIDGen}
import fennec.examples.PingKernel.{Event, State, iterations, kernel}
import org.legogroup.woof.{given, *}
import fennec.KernelCatsSupport.given
import fennec.UpdateEffect
import fs2.concurrent.Topic
import fs2.dom.HtmlElement

import scala.annotation.nowarn

@nowarn
class PingApp[F[_] : Async : Dispatcher : UUIDGen : Logger : LocalStorage](
                                                                            selfSource: Map[String, String],
                                                                          )(using html: Html[F])
  extends FennecApp[F, State, Event](kernel.withEffect(PingApp.effect[F]), selfSource):

  import html.{given, *}


  private val buttonClass =
    cls := "m-2 transition bg-blue-500 hover:bg-green-500 text-white font-bold py-2 px-4 rounded-full hover:scale-150 disabled:opacity-25"


  override def render(outgoing: Topic[F, Event], states: fs2.Stream[F, State]): Resource[F, HtmlElement[F]] =
    div(
      button("start", buttonClass, onClick.as(Event.Start) --> outgoing.publish, disabled <-- states.map(_.isStarted).holdResource(false)),
      br(()),
      states.changesBy(_.isStarted).map(state => p(s"Is started?: ${state.isStarted}")).holdOptionResource,
      states.changesBy(_.stoppedEpochMillis).map(s => div(s"Roundtrip time based on ${iterations} iterations: ${calculatePing(s).getOrElse("N/A")} milliseconds")).holdOptionResource
    )

  def calculatePing(s: State): Option[Double] = s match
    case State(_, Some(startedMillis), Some(stoppedMillis), _) => Some((stoppedMillis - startedMillis).toDouble / iterations)
    case _ => None

end PingApp

object PingApp:

  def effect[F[_] : Applicative : Clock : Logger]: UpdateEffect[F, State, Event, Unit] =
    _ => state =>
      case Event.Pong if state.count <= iterations && state.isStarted => List(Event.Ping).pure[F]
      case Event.Pong if state.count > iterations && state.isStarted => List(Event.Stop).pure[F]
      case Event.Start => Clock[F].realTime.map(now => Event.Started(now.toMillis)).map(e => List(e, Event.Ping))
      case Event.Stop => Clock[F].realTime.map(now => Event.Stopped(now.toMillis)).map(List(_))
      case Event.Stopped(_) => Logger[F].info(s"Stopped: ${state}").as(List.empty)
      case _ => List.empty.pure[F]

end PingApp

