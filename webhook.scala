//> using dep com.indoorvivants::decline-derive::0.3.1
//> using dep org.http4s::http4s-ember-server::0.23.32
//> using dep org.http4s::http4s-ember-client::0.23.32
//> using dep org.http4s::http4s-dsl::0.23.32
//> using dep org.http4s::http4s-circe::0.23.32
//> using dep com.outr::scribe-cats::3.17.0
//> using dep io.circe::circe-jawn::0.14.15
//> using dep io.circe::circe-optics::0.15.1
//> using dep io.circe::circe-literal::0.14.15

import org.http4s.ember.server.*
import org.http4s.*, dsl.io.*, implicits.*
import cats.effect.*
import decline_derive.CommandApplication
import com.comcast.ip4s.*
import com.monovore.decline.Argument
import cats.data.*, cats.syntax.all.*
import decline_derive.*
import org.typelevel.ci.CIString
import org.http4s.circe.*
import cats.effect.std.Supervisor
import concurrent.duration.*
import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import org.http4s.client.Client
import java.time.format.DateTimeFormatter
import java.time.Instant
import java.time.ZoneOffset
import org.http4s.ember.client.EmberClientBuilder

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
    secret: String,
    @Env("KUBE_API", "")
    @Name("kube-api")
    kubeAPI: String,
    @Name("delay")
    delaySeconds: Option[FiniteDuration]
) derives CommandApplication

object Webhook extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val cli = CommandApplication.parseOrExit[CLI](args, sys.env)

    val labelsMapping = Map(
      "indoorvivants/mimalyzer"  -> List("mimalyzer", "mimalyzer-worker"),
      "indoorvivants/scala-boot" -> List("scala-boot"),
      "indoorvivants/sn-bindgen-web-server" -> List("sn-bindgen-web-server"),
      "indoorvivants/sn-bindgen-web-worker" -> List("sn-bindgen-web-worker"),
      "indoorvivants/webhook-kube-deployer" -> List(
        "webhook-kube-deployer"
      )
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

    val delay = cli.delaySeconds.getOrElse(30.seconds)

    def routes(sv: Supervisor[IO], api: MiniKubernetesAPI) = HttpRoutes.of[IO]:
      case req @ POST -> Root / "webhook" =>
        req.body.compile.toVector
          .map(_.toArray)
          .flatMap(bytes =>
            IO
              .fromEither(io.circe.jawn.parseByteArray(bytes))
              .map(bytes -> _)
          )
          .flatMap: (bytes, json) =>
            val digest = hash(cli.secret, bytes)
            val header = req.headers
              .get(CIString("X-Hub-Signature-256"))
              .map(_.head.value)

            header match
              case None => IO(Response(Unauthorized))
              case Some(h) =>
                if digest == h.stripPrefix("sha256=") then
                  import io.circe.optics.JsonPath.*
                  root.action.string.getOption(json) match
                    case Some("published") =>
                      val namespace =
                        for
                          namespace <- root.`package`.namespace.string
                            .getOption(json)
                          name <- root.`package`.name.string.getOption(json)
                          _ = Log.info(s"$namespace, $name")
                        yield namespace + "/" + name

                      namespace.flatMap(labelsMapping.get) match
                        case Some(labels) =>
                          for
                            _ <- Log.info(
                              s"Scheduling a restart of ${labels.mkString(", ")} in ${delay.toSeconds} seconds"
                            )
                            _ <- sv.supervise(
                              (Log
                                .info(
                                  s"Restarting ${labels.mkString(", ")}"
                                ) *> api.restartLabels("default", labels))
                                .delayBy(delay)
                            )
                            resp <- Ok()
                          yield resp
                        case None =>
                          Log
                            .warn(
                              s"Unknown package ${root.`package`.json.getOption(json)}"
                            ) *> Ok()
                      end match
                    case _ => Ok()
                  end match
                else IO(Response(Unauthorized))
            end match

    val api = EmberClientBuilder
      .default[IO]
      .build
      .map(cl => MiniKubernetesAPI(Uri.unsafeFromString(cli.kubeAPI), cl))

    Supervisor[IO]
      .product(api)
      .flatMap: (sv, api) =>
        EmberServerBuilder
          .default[IO]
          .withHost(cli.host.getOrElse(host"0.0.0.0"))
          .withPort(cli.port.getOrElse(port"8080"))
          .withHttpApp(handleErrors(routes(sv, api)))
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

class MiniKubernetesAPI(url: Uri, client: Client[IO]):
  private case class DeploymentsList(items: List[Deployment])
      derives io.circe.Codec.AsObject
  case class AppLabels(app: Option[String]) derives io.circe.Codec.AsObject
  case class Metadata(name: String, labels: AppLabels)
      derives io.circe.Codec.AsObject
  case class Deployment(metadata: Metadata) derives io.circe.Codec.AsObject
  def listDeployments(namespace: String): IO[List[Deployment]] =
    import org.http4s.circe.CirceEntityDecoder.*
    client
      .expect[DeploymentsList](
        url / "apis" / "apps" / "v1" / "namespaces" / namespace / "deployments"
      )
      .map(_.items)
  end listDeployments

  def restartLabels(namespace: String, labels: List[String]) =
    listDeployments(namespace)
      .flatTap(ds =>
        Log.info(s"Found ${ds.map(_.metadata.name).mkString(", ")} deployments")
      )
      .map(
        _.filter(d => labels.contains(d.metadata.labels.app.getOrElse("")))
      )
      .flatTap(ds =>
        Log.info(
          s"Found ${ds.map(_.metadata.name).mkString(", ")} deployments matching any of the labels: ${labels
              .mkString(", ")}"
        )
      )
      .flatMap(_.traverse: deployment =>
        restartDeployment(namespace, deployment.metadata.name))

  def restartDeployment(namespace: String, name: String) =
    import io.circe.*, literal.*
    val annotation = "kubectl.kubernetes.io/restartedAt"
    val df         = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    val newValue = Instant
      .now()
      .atOffset(ZoneOffset.UTC)
      .format(df)
    val patch =
      json"""
        {
          "spec": {
            "template": {
              "metadata": {
                "annotations": {
                  $annotation: $newValue
                }
              }
            }
          }
        }
      """

    import org.http4s.headers.`Content-Type`
    val request = Request[IO]()
      .withMethod(Method.PATCH)
      .withEntity(patch.noSpaces)
      .withContentType(
        `Content-Type`(
          MediaType.unsafeParse("application/strategic-merge-patch+json")
        )
      )
      .withUri(
        url / "apis" / "apps" / "v1" / "namespaces" / namespace / "deployments" / name
      )

    client
      .run(
        request
      )
      .use(resp => resp.bodyText.compile.string.flatMap(b => Log.info(b)))
  end restartDeployment

end MiniKubernetesAPI
