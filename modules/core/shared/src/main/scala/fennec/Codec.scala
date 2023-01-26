package fennec

import java.util.UUID

import cats.Applicative
import cats.data.{IndexedStateT, StateT}
import cats.syntax.all.*
import scala.util.Try

import Codec.*

trait Codec[T]:
  self =>

  def encode(t: T): Vector[Byte]
  def decode: DecoderT[T]
  def dimap[U](contra: U => T, f: T => U): Codec[U] =
    new Codec[U]:
      override def encode(t: U): Vector[Byte] = self.encode(contra(t))
      override def decode: DecoderT[U]        = self.decode.map(f)
end Codec

object Codec:

  export CodecDerived.given Codec

  type EitherThrowable[T] = Either[Throwable, T]

  type DecoderT[T] = StateT[EitherThrowable, Vector[Byte], T]
  object DecoderT:
    def apply: DecoderT[Unit]                           = ().pure[DecoderT]
    def set(s: Vector[Byte]): DecoderT[Unit]            = StateT.set(s)
    def liftF[T](ft: Either[Throwable, T]): DecoderT[T] = StateT.liftF(ft)
    def const[T](ft: T): DecoderT[T]                    = StateT.liftF(ft.asRight)
    def error[T](message: String): DecoderT[T]          = liftF(new Exception(message).asLeft)
    def wrapError[T](throwable: Throwable): DecoderT[T] = liftF(throwable.asLeft)
    def consume: DecoderT[Vector[Byte]] =
      for
        bytes <- StateT.get[EitherThrowable, Vector[Byte]]
        _     <- StateT.set[EitherThrowable, Vector[Byte]](Vector.empty[Byte])
      yield bytes

    def fromEither[T](e: Either[Throwable, T]): DecoderT[T] = e.fold(wrapError, const)
    def fromTry[T](t: Try[T]): DecoderT[T]                  = t.fold(wrapError, const)
    def fromTuple[T](e: (Vector[Byte], T)): DecoderT[T] =
      e match
        case (bytes, t) => StateT.set[EitherThrowable, Vector[Byte]](bytes).map(_ => t)

    def fromEitherTuple[T](e: Either[Throwable, (Vector[Byte], T)]): DecoderT[T] =
      e match
        case Left(value)  => wrapError(value)
        case Right(value) => fromTuple(value)

    val peek: DecoderT[Vector[Byte]] = apply.get
    val readConst: Byte => DecoderT[Byte] = (c: Byte) =>
      byte.flatMap {
        case b if b == c => const(b)
        case b           => error(s"Expected $c but found $b")
      }

    val readBytes: Int => DecoderT[Vector[Byte]] = n =>
      for
        bytes <- apply.get
        value: DecoderT[Vector[Byte]] =
          if bytes.length < n then error("Reached end prematurely") else const(bytes.take(n))
        bytess <- value
        _      <- apply.modify(_.drop(n))
      yield bytess

    val byte: DecoderT[Byte] =
      for
        bytes <- apply.get
        byte <- bytes.headOption.fold[DecoderT[Byte]](error(s"Reached end prematurely: $bytes"))(b => DecoderT.const(b))
        _    <- apply.modify(_.drop(1))
      yield byte

    def repeat[T](n: Int)(d: DecoderT[T]): DecoderT[Vector[T]] =
      Vector.fill(n)(d).sequence

    val long: DecoderT[Long] =
      repeat(8)(byte).map(bytes => BigInt(bytes.toArray).longValue)

    val int: DecoderT[Int] =
      repeat(4)(byte).map(bytes => BigInt(bytes.toArray).intValue)

    val short: DecoderT[Short] =
      repeat(2)(byte).map(bytes => BigInt(bytes.toArray).shortValue)

    val uuid: DecoderT[UUID] =
      for
        lsb <- long
        msb <- long
      yield UUID(msb, lsb)

  end DecoderT

  def apply[T](using c: Codec[T]): Codec[T] = c

end Codec
