package fennec.client

import fennec.KernelSession

case class UserState[State, User](
    index: Long,
    state: State,
    session: KernelSession[User],
)
