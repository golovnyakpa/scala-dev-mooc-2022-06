package module4.http4s_homework

import cats.effect.{IO, Ref, Resource}
import cats.implicits._
import com.comcast.ip4s.{Host, Port}
import module4.http4s_homework.Controllers.{counterService, slowChunkService}
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router

object HTTPServer {

  def routes(counter: Ref[IO, Long]): HttpRoutes[IO] =
    Router("/" -> (counterService(counter) <+> slowChunkService))

  val server = for {
    counter <- Resource.eval(Ref.of[IO, Long](0))
    s <- EmberServerBuilder
           .default[IO]
           .withPort(Port.fromInt(8080).get)
           .withHost(Host.fromString("localhost").get)
           .withHttpApp(routes(counter).orNotFound)
           .build
  } yield s

}
