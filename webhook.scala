//> using dep com.indoorvivants::decline-derive::0.3.1
//> using dep org.http4s::http4s-ember-server::0.23.32
//> using dep org.http4s::http4s-dsl::0.23.32
//> using dep org.http4s::http4s-circe::0.23.32
//> using dep com.outr::scribe-cats::3.17.0
//> using dep io.circe::circe-optics::0.15.0

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

    val routes = HttpRoutes.of[IO]:
      case req @ POST -> Root / "webhook" =>
        req.asJson.flatMap: json =>
          import io.circe.optics.JsonPath.*
          root.action.string.getOption(json) match
            case Some("published") =>
              IO.println(json) *> Ok("got it")
            case _ => Ok()

    EmberServerBuilder
      .default[IO]
      .withHost(cli.host.getOrElse(host"0.0.0.0"))
      .withPort(cli.port.getOrElse(port"8080"))
      .withHttpApp(handleErrors(routes))
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
