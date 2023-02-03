package fennec.examples

import cats.effect.kernel.{Async, Resource, Sync}
import cats.effect.std.Dispatcher
import fs2.concurrent.Topic
import org.scalajs.dom
import org.scalajs.dom.document

object KeyboardInput:

  enum KeyboardEvent:
    case Number(i: Int)
    case Other(code: Int, name: String)

  def sub[F[_]: Async]: Resource[F, Topic[F, KeyboardEvent]] =
    for
      topic      <- Resource.eval(Topic[F, KeyboardEvent])
      dispatcher <- Dispatcher.parallel[F]
      _ <- Resource.eval(
        Sync[F].delay(
          document.addEventListener(
            `type` = "keydown",
            listener = event =>
              val kbe  = event.asInstanceOf[dom.KeyboardEvent]
              val name = kbe.key
              val code = kbe.keyCode
              val evt =
                if 48.to(57).contains(code) then KeyboardEvent.Number(code - 48)
                else KeyboardEvent.Other(code, name)
              dispatcher.unsafeRunAndForget(topic.publish1(evt)),
          ),
        ),
      )
    yield topic

end KeyboardInput
