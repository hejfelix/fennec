package fennec

import java.util.UUID
import scala.annotation.nowarn

enum Message[+S, +E, +User]:
  case RequestSession(id: Option[UUID])
  case AuthenticateSession(id: UUID, proof: Proof)
  case SessionAuthenticated(id: UUID, proof: Proof, user: User)
  case AuthenticationFailed(id: UUID, proof: Proof, message: String)
  case SessionHandshake(initState: S, sessionId: UUID)
  case EventMessage(sessionCounter: Long, event: E)
  case Acknowledge(sessionCounter: Long)
  case SharedEvent[E](origin: UUID, event: E) extends Message[Nothing, E, Nothing]
end Message

@nowarn("msg=nused explicit parameter") // wat even do you mean, compiler?
enum Direction:
  case Outgoing
  case Incoming
