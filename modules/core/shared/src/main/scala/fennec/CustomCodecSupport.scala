package fennec

import Codec.*
import Message.*

object CustomCodecSupport:
  import CodecDefaults.given_Codec_String

  def codecVia[A, B: Codec](f: A => B, g: B => A): Codec[A] =
    new Codec[A]:
      override def encode(t: A): Vector[Byte] =
        Codec[B].encode(f(t))
      override def decode: DecoderT[A] =
        Codec[B].decode.map(g)

  given proofCodec: Codec[Proof] with

    object Headers:
      val USER_PASSWORD: Byte = 0x01
      val TOKEN: Byte         = 0x02

    override def encode(t: Proof): Vector[Byte] =
      t match
        case Proof.UserPassword(user, password) =>
          Vector(Headers.USER_PASSWORD) ++ Codec[String].encode(user) ++ Codec[String].encode(
            password,
          )
        case Proof.Token(bytes) =>
          Vector(Headers.TOKEN) ++ CodecDefaults.given_Codec_Int.encode(bytes.length) ++ bytes
    override def decode: DecoderT[Proof] = DecoderT.byte.flatMap(usingHeader)

    def usingHeader(header: Byte): DecoderT[Proof] =
      header match
        case Headers.USER_PASSWORD =>
          for
            user <- Codec[String].decode
            pass <- Codec[String].decode
          yield Proof.UserPassword(user, pass)

        case Headers.TOKEN =>
          for
            n     <- DecoderT.int
            bytes <- DecoderT.readBytes(n)
          yield Proof.Token(bytes)
  end proofCodec

  def messageCodec[S: Codec, E: Codec, U: Codec]: Codec[Message[S, E, U]] =
    new Codec[Message[S, E, U]]:

      object Headers:
        val REQUEST_NEW_SESSION: Byte   = 0x01
        val REQUEST_SESSION: Byte       = 0x02
        val SESSION_HANDSHAKE: Byte     = 0x03
        val EVENT_MESSAGE: Byte         = 0x04
        val ACK: Byte                   = 0x05
        val SHARED_EVENT: Byte          = 0x06
        val AUTHENTICATE_SESSION: Byte  = 0x07
        val SESSION_AUTHENTICATED: Byte = 0x08
        val AUTHENTICATION_FAILED: Byte = 0x09
      end Headers

      override def encode(t: Message[S, E, U]): Vector[Byte] =
        t match
          case RequestSession(None) => Vector(Headers.REQUEST_NEW_SESSION)
          case RequestSession(Some(id)) =>
            Vector(Headers.REQUEST_SESSION) ++ CodecDefaults.given_Codec_UUID.encode(id)
          case SessionHandshake(initState, sessionId) =>
            Vector(Headers.SESSION_HANDSHAKE) ++
              CodecDefaults.given_Codec_UUID.encode(sessionId) ++
              Codec[S].encode(initState)
          case EventMessage(sessionCounter, event) =>
            val codecE = Codec[E]
            Vector(Headers.EVENT_MESSAGE) ++ CodecDefaults.given_Codec_Long.encode(
              sessionCounter,
            ) ++ codecE.encode(
              event,
            )
          case Acknowledge(sessionCounter) =>
            Vector(Headers.ACK) ++ CodecDefaults.given_Codec_Long.encode(sessionCounter)
          case SharedEvent(origin, event) =>
            Vector(Headers.SHARED_EVENT) ++ CodecDefaults.given_Codec_UUID
              .encode(origin) ++ Codec[E].encode(event)
          case AuthenticateSession(id, proof) =>
            Vector(Headers.AUTHENTICATE_SESSION) ++ CodecDefaults.given_Codec_UUID.encode(
              id,
            ) ++ Codec[Proof].encode(
              proof,
            )
          case SessionAuthenticated(id, proof, user) =>
            Vector(Headers.SESSION_AUTHENTICATED) ++ CodecDefaults.given_Codec_UUID.encode(
              id,
            ) ++ Codec[Proof].encode(
              proof,
            ) ++ Codec[U].encode(
              user,
            )
          case AuthenticationFailed(id, proof, message) =>
            Vector(Headers.AUTHENTICATION_FAILED) ++ CodecDefaults.given_Codec_UUID.encode(
              id,
            ) ++ Codec[Proof].encode(
              proof,
            ) ++ Codec[String]
              .encode(message)

      override def decode: DecoderT[Message[S, E, U]] =
        DecoderT.byte.flatMap(usingHeader)

      def usingHeader(header: Byte): DecoderT[Message[S, E, U]] =
        header match
          case Headers.REQUEST_NEW_SESSION => DecoderT.apply.map(_ => RequestSession(None))
          case Headers.REQUEST_SESSION     => DecoderT.uuid.map(id => RequestSession(Some(id)))
          case Headers.SESSION_HANDSHAKE =>
            for
              id    <- DecoderT.uuid
              state <- Codec[S].decode
            yield SessionHandshake(state, id)
          case Headers.EVENT_MESSAGE =>
            for
              counter <- DecoderT.long
              event   <- Codec[E].decode
            yield EventMessage(counter, event)
          case Headers.ACK => DecoderT.long.map(Acknowledge(_))
          case Headers.SHARED_EVENT =>
            for
              id    <- DecoderT.uuid
              event <- Codec[E].decode
            yield SharedEvent(id, event)
          case Headers.AUTHENTICATE_SESSION =>
            for
              id    <- DecoderT.uuid
              proof <- Codec[Proof].decode
            yield AuthenticateSession(id, proof)
          case Headers.SESSION_AUTHENTICATED =>
            for
              id    <- DecoderT.uuid
              proof <- Codec[Proof].decode
              user  <- Codec[U].decode
            yield SessionAuthenticated(id, proof, user)
          case Headers.AUTHENTICATION_FAILED =>
            for
              id      <- DecoderT.uuid
              proof   <- Codec[Proof].decode
              message <- Codec[String].decode
            yield AuthenticationFailed(id, proof, message)

end CustomCodecSupport
