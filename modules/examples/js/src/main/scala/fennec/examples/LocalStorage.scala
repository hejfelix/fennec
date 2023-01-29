package fennec.examples

import cats.effect.kernel.Sync
import org.scalajs.dom.window

trait LocalStorage[F[_]: Sync]:
  def getItem(key: String): F[Option[String]]
  def setItem(key: String, value: String): F[Unit]
end LocalStorage

object LocalStorage:

  def apply[F[_]](using l: LocalStorage[F]): LocalStorage[F] = l

  given default[F[_]: Sync]: LocalStorage[F] = new:
    override def getItem(key: String): F[Option[String]] = 
      Sync[F].delay{
        val value = window.localStorage.getItem(key)
        if value == null then None else Some(value)
      }
    override def setItem(key: String, value: String): F[Unit] =
      Sync[F].delay(window.localStorage.setItem(key, value))
end LocalStorage
