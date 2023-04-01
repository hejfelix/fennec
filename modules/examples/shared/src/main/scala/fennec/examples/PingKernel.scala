package fennec.examples

import cats.Id
import fennec.{CodecDerived, Kernel, Update}

import scala.annotation.nowarn
import fennec.CodecDerived.given
import fennec.CodecDefaults.given

object PingKernel:

  @nowarn
  enum Event:
    case Start
    case Stop
    case Reset
    case Started(epochMillis: Long)
    case Stopped(epochMillis: Long)
    case Ping
    case Pong

  case class State(count: Int, startedEpochMillis: Option[Long], stoppedEpochMillis: Option[Long], isStarted: Boolean)

  val iterations = 1000

  val update: Update[State, Event] = state =>
    case Event.Ping => state.copy(count = state.count + 1)
    case Event.Start => state.copy(isStarted = true, count = 0)
    case Event.Stop => state.copy(isStarted = false, count = 0)
    case Event.Stopped(stoppedMillis) => state.copy(stoppedEpochMillis = Some(stoppedMillis))
    case Event.Started(startedMillis) => state.copy(startedEpochMillis = Some(startedMillis))
    case Event.Reset => initialState
    case _ => state

  val initialState: State = State(count = 0, startedEpochMillis = None, stoppedEpochMillis = None, isStarted = false)

  val kernel: Kernel[Id, State, Event, Unit] =
    Kernel
      .init[State, Event](name = "ping", initialState)
      .withUpdate(update)

end PingKernel

