package fennec

import fennec.Codec.DecoderT

import java.nio.ByteBuffer
import java.util.UUID
import cats.syntax.all.*

import java.nio.charset.StandardCharsets

object CodecDefaults:

  given Codec[Char]                                        = Codec[Byte].dimap(_.toByte, _.toChar)
  given [A: Codec]: Codec[Set[A]]                          = Codec[List[A]].dimap(_.toList, _.toSet)
  given [Key: Codec, Value: Codec]: Codec[Map[Key, Value]] = Codec[List[(Key, Value)]].dimap(_.toList, _.toMap)

  given Codec[Byte] with
    override def encode(t: Byte): Vector[Byte] = Vector(t)
    override def decode: DecoderT[Byte]        = DecoderT.byte

  given [A: Codec]: Codec[List[A]] with
    override def encode(t: List[A]): Vector[Byte] =
      Codec[Int].encode(t.size) ++ t.flatMap(Codec[A].encode)
    override def decode: DecoderT[List[A]] =
      for
        size <- Codec[Int].decode
        as   <- (0 until size).toList.traverse(_ => Codec[A].decode)
      yield as

  given [A: Codec, B: Codec]: Codec[(A, B)] with
    override def encode(t: (A, B)): Vector[Byte] = Codec[A].encode(t._1) ++ Codec[B].encode(t._2)
    override def decode: DecoderT[(A, B)] =
      for
        a <- Codec[A].decode
        b <- Codec[B].decode
      yield (a, b)

  given Codec[Unit] with
    override def encode(t: Unit): Vector[Byte] = Vector(0x00)
    override def decode: DecoderT[Unit]        = DecoderT.readConst(0x00).as(())

  given Codec[Boolean] with
    override def encode(t: Boolean): Vector[Byte] = Vector(if t then 0x01 else 0x00)
    override def decode: DecoderT[Boolean] = DecoderT.byte.flatMap(
      _ match
        case 0x00 => DecoderT.const(false)
        case 0x01 => DecoderT.const(true)
        case b    => DecoderT.error(s"Expected boolean, but got $b"),
    )

  given Codec[Int] with
    def encode(t: Int): Vector[Byte] = ByteBuffer.allocate(4).putInt(t).array().toVector
    def decode: DecoderT[Int]        = DecoderT.int

  given Codec[Long] with
    def encode(t: Long): Vector[Byte] = ByteBuffer.allocate(8).putLong(t).array().toVector
    def decode: DecoderT[Long]        = DecoderT.long

  given Codec[Short] with
    override def encode(t: Short): Vector[Byte] = ByteBuffer.allocate(2).putShort(t).array().toVector
    override def decode: DecoderT[Short]        = DecoderT.short

  given Codec[UUID] with
    override def encode(t: UUID): Vector[Byte] = BigInt(t.getLeastSignificantBits).toByteArray.toVector ++ BigInt(
      t.getMostSignificantBits,
    ).toByteArray.toVector
    override def decode: DecoderT[UUID] = DecoderT.uuid

  given Codec[String] with
    override def encode(t: String): Vector[Byte] =
      val bytes = t.getBytes(StandardCharsets.UTF_8)
      CodecDefaults.given_Codec_Int.encode(bytes.length) ++ bytes
    override def decode: DecoderT[String] =
      for
        i     <- DecoderT.int
        bytes <- DecoderT.readBytes(i)
      yield new String(bytes.toArray, StandardCharsets.UTF_8)
  end given

end CodecDefaults
