import Log.*
import cats.effect.*
import cats.effect.std.Supervisor
import io.circe.ParsingFailure
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.typelevel.ci.CIString

import concurrent.duration.*
import io.circe.DecodingFailure

case class Package(name: String, namespace: String)
    derives io.circe.Codec.AsObject
case class PublishedEvent(action: String, `package`: Option[Package])
    derives io.circe.Codec.AsObject

class Restarter(
    sv: Supervisor[IO],
    api: MiniKubernetesAPI,
    cli: CLI,
    labelsMapping: Map[String, List[String]]
):
  val routes = HttpRoutes.of[IO]:
    case _ @GET -> Root / "health" =>
      api
        // make sure the kube connectivity is okay
        .listDeployments("default")
        .flatMap: ds =>
          Ok(
            io.circe.Json.obj(
              "status"      -> io.circe.Json.fromString("ok"),
              "deployments" -> io.circe.Json.fromInt(ds.size)
            )
          )
    case req @ POST -> Root / "webhook" =>
      req.body.compile.toVector
        .map(_.toArray)
        .flatMap(bytes =>
          IO
            .fromEither(io.circe.jawn.parseByteArray(bytes))
            .map(bytes -> _)
        )
        .flatMap: (bytes, json) =>
          val digest = Crypto.hash(cli.secret, bytes)
          val header = req.headers
            .get(CIString("X-Hub-Signature-256"))
            .map(_.head.value)

          if !header.contains("sha256=" + digest) then BadRequest("invalid signature")
          else
            IO.fromEither(json.as[PublishedEvent])
              .flatMap:
                case PublishedEvent("published", Some(pkg)) =>
                  handlePackage(pkg)
                case PublishedEvent(other, pkg) =>
                  info(s"ignoring $other, $pkg event") *> NoContent()
          end if
        .recoverWith:
          case p: ParsingFailure  => BadRequest(s"Invalid JSON ${p.message}")
          case p: DecodingFailure => BadRequest(s"Invalid JSON ${p.message}")

  def handlePackage(p: Package) =
    val namespace = p.namespace + "/" + p.name
    labelsMapping.get(namespace) match
      case Some(labels) =>
        for
          _ <- info(
            s"Scheduling a restart of ${labels.mkString(", ")} in ${delay.toSeconds} seconds"
          )
          publishing =
            info(s"Restarting ${labels.mkString(", ")}") *>
              api
                .restartLabels("default", labels)

          _    <- sv.supervise(publishing.delayBy(delay))
          resp <- Ok()
        yield resp
      case None =>
        warn(s"Unknown package $namespace") *> Ok()
    end match
  end handlePackage

  val delay = cli.delaySeconds.getOrElse(10.seconds)
end Restarter
