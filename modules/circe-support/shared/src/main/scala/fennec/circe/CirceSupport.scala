package fennec.circe

import fennec.Codec.DecoderT
import fennec.{Codec, CodecDefaults}
import io.circe.{Decoder, Encoder}

object CirceSupport:

  given [T](using Encoder[T], Decoder[T]): Codec[T] with
    val cc = io.circe.Codec.from(Decoder[T], Encoder[T])

    def encode(t: T): Vector[Byte] =
      CodecDefaults.given_Codec_String.encode(cc.apply(t).noSpaces)

    def decode: DecoderT[T] =
      for
        str <- CodecDefaults.given_Codec_String.decode
        r   <- DecoderT.fromEither(io.circe.parser.decode[T](str))
      yield r
  end given
end CirceSupport
