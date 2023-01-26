package fennec

import scala.util.{Failure, Try}
import cats.syntax.all.given

object CodecDerived:
  import Codec.*

  import scala.deriving.*
  import scala.compiletime.{erasedValue, summonInline}

  inline def summonAll[T <: Tuple]: List[Codec[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Codec[t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.Of[T]): Codec[T] =
    val allInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => sumOf(s, allInstances)
      case p: Mirror.ProductOf[T] => productOf(p, allInstances)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def productOf[T](m: Mirror.ProductOf[T], instances: List[Codec[?]]): Codec[T] =
    new Codec[T]:
      override def encode(t: T): Vector[Byte] =
        instances.zip(iterator(t)).foldMap((instance, param) => instance.asInstanceOf[Codec[Any]].encode(param))
      override def decode: DecoderT[T] =
        instances
          .map(_.asInstanceOf[Codec[Any]])
          .traverse(_.decode)
          .map(_.toArray)
          .map(Tuple.fromArray)
          .map(m.fromProduct)

  def sumOf[T](m: Mirror.SumOf[T], instances: List[Codec[?]]): Codec[T] = new Codec[T]:
    override def encode(t: T): Vector[Byte] =
      CodecDefaults.given_Codec_Short
        .encode(m.ordinal(t).toShort) ++ instances(m.ordinal(t)).asInstanceOf[Codec[Any]].encode(t)
    override def decode: DecoderT[T] =
      for
        ordinal <- DecoderT.short
        res     <- instances(ordinal).asInstanceOf[Codec[Any]].decode
      yield res.asInstanceOf[T]

end CodecDerived
