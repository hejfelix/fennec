package fennec

import cats.implicits.*
import fennec.{Codec, CustomCodecSupport}

import java.util.UUID
import CustomCodecSupport.*
import CustomCodecSupportSuite.*
import Codec.DecoderT

import weaver.{given, *}
import cats.kernel.Eq
import java.nio.charset.StandardCharsets

trait CodecTest:
  self: FunSuite =>
  given codec: Codec[Message[MyTestState, MyTestEvent, User]]

  def reEncode[T: Codec](t: T) =
    expect(Codec[T].decode.run(Codec[T].encode(t)).map(_._2) == Right(t))

  import Message.*
  val testCases: List[Message[MyTestState, MyTestEvent, User]] = List(
    RequestSession(None),
    RequestSession(Some(UUID.randomUUID())),
    SessionHandshake(MyTestState(42), UUID.randomUUID()),
    EventMessage(1337L, MyTestEvent('q')),
    Acknowledge(42L),
    SharedEvent(UUID.randomUUID(), MyTestEvent('z')),
    AuthenticateSession(UUID.randomUUID(), Proof.UserPassword("user", "loong12312!password")),
    AuthenticateSession(UUID.randomUUID(), Proof.Token(Vector(1, 2, 3, 4).map(_.toByte))),
    SessionAuthenticated(UUID.randomUUID(), Proof.UserPassword("user", "loong12312!password"), User("Alan")),
    AuthenticationFailed(UUID.randomUUID(), Proof.UserPassword("user", "loong12312!password"), "unauthorized"),
  )

  testCases.foreach(c =>
    test(c.toString) {
      reEncode(c)
    },
  )

  test("long codec") {
    val cod    = CodecDefaults.given_Codec_Long
    val result = cod.decode.run(cod.encode(1337L))
    expect(result == (Vector.empty, 1337L).asRight)
  }
end CodecTest

object CustomCodecSupportSuite extends FunSuite with CodecTest:
  case class MyTestState(id: Int)
  case class MyTestEvent(char: Char)
  case class User(name: String)

  import CodecDerived.given
  import CodecDefaults.given
  import fennec.CustomCodecSupport.messageCodec
  override given codec: Codec[Message[MyTestState, MyTestEvent, User]] = messageCodec[MyTestState, MyTestEvent, User]

end CustomCodecSupportSuite
