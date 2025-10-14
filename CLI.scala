import cats.data.ValidatedNel
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.monovore.decline.Argument
import decline_derive.{CommandApplication, *}

import concurrent.duration.*

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

