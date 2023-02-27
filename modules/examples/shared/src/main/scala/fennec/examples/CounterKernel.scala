package fennec.examples

import fennec.{Kernel, Update}

import scala.annotation.nowarn
import fennec.CodecDerived.given
import fennec.CodecDefaults.given

object CounterKernel:

  case class State(count: Int)

  @nowarn("msg=unused explicit parameter")
  enum Event:
    case Increment
    case Decrement

  val update: Update[State, Event] = state =>
    case Event.Increment => State(state.count + 1)
    case Event.Decrement => State(state.count - 1)

  val kernel: Kernel[cats.Id, State, Event, Unit] =
    Kernel
      .init[State, Event]("counter", State(0))
      .withUpdate(update)

end CounterKernel
