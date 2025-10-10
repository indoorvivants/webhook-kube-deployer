//> using dep com.indoorvivants::decline-derive::0.3.1
//> using dep org.http4s::http4s-ember-server::0.23.32
//> using dep org.http4s::http4s-dsl::0.23.32
//> using dep org.http4s::http4s-circe::0.23.32
//> using dep com.outr::scribe-cats::3.17.0
//> using dep io.circe::circe-jawn::0.14.15
//> using dep io.circe::circe-optics::0.15.1

import org.http4s.ember.server.*
import org.http4s.*, dsl.io.*, implicits.*
import cats.effect.*
import decline_derive.CommandApplication
import com.comcast.ip4s.*
import com.monovore.decline.Argument
import cats.data.*, cats.syntax.all.*
import decline_derive.Env
import org.typelevel.ci.CIString
import org.http4s.circe.*
import cats.effect.std.Supervisor
import concurrent.duration.*
import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac

extension [A, B](x: Argument[A])
  def mapValidated(f: A => ValidatedNel[String, B]): Argument[B] =
    new Argument[B]:
      override def read(string: String): ValidatedNel[String, B] =
        x.read(string).andThen(f)

      override def defaultMetavar: String = x.defaultMetavar

given Argument[Port] =
  Argument.readInt.mapValidated(p =>
    Port.fromInt(p).toRight(s"Invalid port $p").toValidatedNel
  )

given Argument[Host] =
  Argument.readString.mapValidated(p =>
    Host.fromString(p).toRight(s"Invalid host $p").toValidatedNel
  )

case class CLI(
    port: Option[Port],
    host: Option[Host],
    @Env("WEBHOOK_SECRET", "")
    secret: Option[String]
) derives CommandApplication

object Webhook extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val cli = CommandApplication.parseOrExit[CLI](args, sys.env)
    def protect(app: HttpRoutes[IO]): HttpRoutes[IO] =
      val decoder = java.util.Base64.getDecoder()
      cli.secret match
        case Some(expectedSecret) =>
          HttpRoutes.of[IO]:
            case req =>
              val decode = req.headers
                .get(CIString("X-Github-Encoded-Secret"))
                .map(_.head.value)
                .map(decoder.decode(_))
                .map(String(_))

              decode match
                case Some(value) if value == expectedSecret =>
                  app(req).getOrElse(Response(NotFound))
                case _ =>
                  Response(
                    Unauthorized.withReason("Invalid or missing webhook secret")
                  ).pure[IO]
              end match

        case None =>
          app
      end match
    end protect

    val labelsMapping = Map(
      "indoorvivants/mimalyzer"  -> List("mimalyzer", "mimalyzer-worker"),
      "indoorvivants/scala-boot" -> List("scala-boot"),
      "indoorvivants/sn-bindgen-web-server" -> List("sn-bindgen-web-server"),
      "indoorvivants/sn-bindgen-web-worker" -> List("sn-bindgen-web-worker"),
      "indoorvivants/webhook-kube-deployer" -> List("webhook-kube-deployer")
    )

    def hash(key: String, bytes: Array[Byte]) =
      val secretKeySpec = SecretKeySpec(key.getBytes(), "HmacSHA256")
      val mac           = Mac.getInstance("HmacSHA256")
      mac.init(secretKeySpec)
      bytesToHex(mac.doFinal(bytes))
    end hash

    def bytesToHex(hash: Array[Byte]): String =
      val hexString = new StringBuilder(2 * hash.length)
      for i <- hash.indices do
        val hex = Integer.toHexString(0xff & hash(i))
        if hex.length() == 1 then hexString.append('0')
        hexString.append(hex)
      hexString.toString()
    end bytesToHex

    def routes(sv: Supervisor[IO]) = HttpRoutes.of[IO]:
      case req @ POST -> Root / "webhook" =>
        req.body.compile.toVector
          .map(_.toArray)
          .flatMap: bytes =>
            val digest = cli.secret.map(hash(_, bytes))
            val header = req.headers
              .get(CIString("X-Hub-Signature-256"))
              .map(_.head.value)

            (digest, header) match
              case (Some(_), None) => IO(Response(Unauthorized))
              case (Some(d), Some(h)) =>
                if d == h.stripPrefix("sha256=") then
                  IO.fromEither(io.circe.jawn.parseByteArray(bytes))
                    .flatMap: json =>

                      import io.circe.optics.JsonPath.*
                      root.action.string.getOption(json) match
                        case Some("published") =>
                          val schedule =
                            (for
                              namespace <- root.`package`.namespace.string
                                .getOption(json)
                              name <- root.`package`.name.string.getOption(json)
                              _ = println(s"$namespace, $name")
                            yield namespace + "/" + name,
                            ).flatMap(labelsMapping.get) match
                              case Some(labels) =>
                                IO.println(
                                  s"Scheduling a restart of ${labels.mkString(", ")} in 30 seconds"
                                )
                                  *> sv.supervise(
                                    IO.println(
                                      s"Restarting ${labels.mkString(", ")}"
                                    ).delayBy(30.seconds)
                                  )
                              case None =>
                                IO.println(
                                  s"Unknown package ${root.`package`.json.getOption(json)}"
                                ).as(Ok)
                            end match
                          end schedule

                          schedule *> Ok()
                        case _ => Ok()
                      end match
                else IO(Response(Unauthorized))
              case (None, _) => InternalServerError()
            end match

    Supervisor[IO]
      .flatMap: sv =>
        EmberServerBuilder
          .default[IO]
          .withHost(cli.host.getOrElse(host"0.0.0.0"))
          .withPort(cli.port.getOrElse(port"8080"))
          .withHttpApp(handleErrors(routes(sv)))
          .build
          .evalTap(ser => IO.println(s"Server started on ${ser.baseUri}"))
      .useForever
      .as(ExitCode.Success)
  end run

  def handleErrors(routes: HttpRoutes[IO]) =
    routes.orNotFound.onError { exc =>
      Kleisli(request => Log.error(s"Request failed: [$request]", exc))
    }
end Webhook

object Log:
  private lazy val io =
    scribe.Logger.root
      .clearHandlers()
      .withHandler(
        writer = scribe.writer.SystemErrWriter,
        outputFormat = scribe.output.format.ANSIOutputFormat
      )
      .replace()

    scribe.cats.io
  end io

  export io.{debug, info, warn, error}

  export scribe.{
    info as infoUnsafe,
    debug as debugUnsafe,
    warn as warnUnsafe,
    error as errorUnsafe
  }
end Log
