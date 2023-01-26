package fennec

import java.util.UUID

given [User]: (User => Unit) = _ => ()

type Update[State, Event]                   = State => Event => State
type UpdateEffect[F[_], State, Event, User] = KernelSession[User] => State => Event => F[List[Event]]

enum KernelSession[+User](val id: UUID):
  case AuthenticatedSession[User](override val id: UUID, proof: Proof, user: User) extends KernelSession[User](id)
  case AnonymousSession(override val id: UUID)                                     extends KernelSession[Nothing](id)

  def map[U](f: User => U): KernelSession[U] = this match
    case a: AnonymousSession               => a.asInstanceOf[KernelSession[U]]
    case a @ AuthenticatedSession(_, _, _) => a.copy(user = f(a.user))

enum Proof:
  case UserPassword(user: String, password: String)
  case Token(bytes: Vector[Byte])
