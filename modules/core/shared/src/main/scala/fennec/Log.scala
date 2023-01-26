package fennec

trait Log[F[_]]:
  def info(msg: String): F[Unit]
object Log:
  def apply[F[_]](using l: Log[F]): Log[F] = l
