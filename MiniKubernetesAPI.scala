import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import Log.*
import MiniKubernetesAPI.*

class MiniKubernetesAPI(url: Uri, client: Client[IO]):

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
        info(s"Found ${ds.map(_.metadata.name).mkString(", ")} deployments")
      )
      .map(
        _.filter(d => labels.contains(d.metadata.labels.app.getOrElse("")))
      )
      .flatTap(ds =>
        info(
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
      .use(resp => resp.bodyText.compile.string.flatMap(b => info(b)))
  end restartDeployment
end MiniKubernetesAPI

object MiniKubernetesAPI:
  private case class DeploymentsList(items: List[Deployment])
      derives io.circe.Codec.AsObject
  case class AppLabels(app: Option[String]) derives io.circe.Codec.AsObject
  case class Metadata(name: String, labels: AppLabels)
      derives io.circe.Codec.AsObject
  case class Deployment(metadata: Metadata) derives io.circe.Codec.AsObject
end MiniKubernetesAPI
