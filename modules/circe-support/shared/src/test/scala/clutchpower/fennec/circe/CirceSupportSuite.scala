package fennec.circe
import fennec.{Codec, CodecDefaults, CustomCodecSupport}
import fennec.circe.CirceSupport.given
import CustomCodecSupport.*

import java.nio.charset.StandardCharsets
import io.circe.syntax.given

import weaver.*

object CirceSupportSuite extends FunSuite:

  test("encoding works") {

    val testString    = "hey"
    val actual          = Codec[String].encode(testString)
    val expectedBytes = testString.asJson.noSpaces.getBytes(StandardCharsets.UTF_8).toVector

    val headerBytes = CodecDefaults.given_Codec_Int.encode(expectedBytes.length)

    expect(actual == headerBytes ++ expectedBytes)
  }

  test("encoding compose decode is identity") {

    val testString = "this is a test"
    val asdf       = Codec[String].encode(testString)
    val decoded    = Codec[String].decode.run(asdf).getOrElse(???)._2

    expect(decoded == testString)
  }
end CirceSupportSuite
