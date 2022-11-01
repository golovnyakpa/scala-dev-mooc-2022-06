package http4s_homework

import cats.effect.{IO, Ref, Resource}
import module4.http4s_homework.HTTPServer
import module4.http4s_homework.Models.Counter
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import org.http4s.{Request, Response}
import org.scalatest.flatspec.AnyFlatSpec
import org.http4s.circe.CirceEntityDecoder._
import cats.effect.unsafe.IORuntime

class HTTPServerSpec extends AnyFlatSpec {

  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  implicit val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val baseUri                                    = uri"http://localhost:8080/"

  "counter" should "give incremental values" in {
    val request = Request[IO](uri = baseUri.addPath("counter"))
    val resp: IO[Option[IO[Counter]]] =
      Ref
        .of[IO, Long](0L)
        .flatMap(ref =>
          HTTPServer
            .routes(ref)
            .run(request)
            .map(resp => resp.as[Counter])
            .value
        )

    assert(resp.unsafeRunSync().map(x => x.unsafeRunSync).contains(Counter(0L))) // выглядит ужасно
    // здесь я не до конца разобрался, ещё и unsafeRunSync
  }

}
