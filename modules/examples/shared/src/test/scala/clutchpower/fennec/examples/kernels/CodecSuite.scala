package clutchpower.fennec.examples.kernels
import cats.syntax.all.*
import fennec.{Codec, FormId}
import fennec.CodecDerived.given
import fennec.CodecDefaults.given
import fennec.Message.SessionHandshake
import java.util.UUID
import weaver.*

object CodecSuite extends FunSuite:
  import fennec.examples.FormsKernel.*
  test("decode handshake") {
    val bytes: Vector[Byte] = Vector(
      3,
      -102,
      -85,
      63,
      -44,
      -126,
      5,
      -28,
      -9,
      123,
      67,
      70,
      18,
      121,
      -70,
      68,
      -80,
      0,
      0,
      0,
      1,
      0,
      0,
      -72,
      -64,
      -83,
      102,
      -74,
      -23,
      89,
      73,
      -28,
      -25,
      -4,
      79,
      -99,
      105,
      71,
      124,
      0,
      0,
      0,
      4,
      97,
      115,
      100,
      102,
      0,
      0,
      0,
      0,
      0,
    ).map(_.toByte)

    val codec: Codec[kernel.M] = kernel.messageCodec

    val expected: kernel.M = SessionHandshake(
      Map(FormId.Id(UUID.fromString("e4e7fc4f-9d69-477c-b8c0-ad66b6e95949")) -> UserInfo("asdf", 0, false)),
      UUID.fromString("7b434612-79ba-44b0-9aab-3fd48205e4f7"),
    )

    val result = codec.decode.run(bytes)
    expect(
      result.map(_._2) ==
        expected.asRight,
    )
  }

  test("state encode decode") {
    val felix        = UserInfo("Felix", 34, false)
    val state: State = Map(FormId.Id(java.util.UUID.randomUUID()) -> felix)
    encodeDecode(state)
  }

  def encodeDecode[T: Codec](t: T): Expectations =
    val bytes: Vector[Byte] = Codec[T].encode(t)
    val decoded             = Codec[T].decode.run(bytes)
    expect(
      decoded.map(_._2) ==
        t.asRight,
    )

end CodecSuite
