import cats.data.*
import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import com.comcast.ip4s.*
import decline_derive.CommandApplication
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.*
import org.http4s.implicits.*

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

    val kube = EmberClientBuilder
      .default[IO]
      .build
      .map(cl => MiniKubernetesAPI(Uri.unsafeFromString(cli.kubeAPI), cl))

    Supervisor[IO]
      .product(kube)
      .flatMap: (sv, api) =>
        EmberServerBuilder
          .default[IO]
          .withHost(cli.host.getOrElse(host"0.0.0.0"))
          .withPort(cli.port.getOrElse(port"8080"))
          .withHttpApp(
            handleErrors(Restarter(sv, api, cli, labelsMapping).routes)
          )
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

