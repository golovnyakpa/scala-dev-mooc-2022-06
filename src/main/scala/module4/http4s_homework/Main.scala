package module4.http4s_homework

import cats.effect.{IO, IOApp, Resource}
import module4.http4s_homework.Controllers.InputParams
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._

object Main extends IOApp.Simple {

  override def run: IO[Unit] = {
    implicit val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
    val baseUri                                    = uri"http://localhost:8080/"
    for {
      counterRes <- HTTPServer.server.use(_ =>
                      HTTPClient
                        .getCounterNTimes(uri = baseUri.addPath("counter"), 5)
                        .product(HTTPClient.getSlowChunkService(baseUri, InputParams("100", "500", "1")))
                    )
      _ <- IO.println(counterRes)

    } yield counterRes
  }

}
