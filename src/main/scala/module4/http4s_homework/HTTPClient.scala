package module4.http4s_homework

import cats.effect.{IO, Resource}
import cats.implicits._
import fs2.text
import module4.http4s_homework.Controllers.InputParams
import module4.http4s_homework.Models.Counter
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}

object HTTPClient {

  def getCounterNTimes(uri: Uri, n: Int)(implicit builder: Resource[IO, Client[IO]]): IO[List[Counter]] = {
    val counterRequest: Request[IO] = Request[IO](uri = uri)
    val result: Resource[IO, List[Response[IO]]] = for {
      client <- builder
      resp   <- client.run(counterRequest).replicateA(n)
    } yield resp

    result.use(lst => lst.parTraverse(resp => resp.as[Counter]))
  }

  def getSlowChunkService(baseUri: Uri, params: InputParams)(implicit
    builder: Resource[IO, Client[IO]]
  ): IO[List[String]] = {
    val uri                          = baseUri.addPath("slow").addPath(params.chunk).addPath(params.total).addPath(params.time)
    val getChunkRequest: Request[IO] = Request[IO](uri = uri)
    val result = for {
      client <- builder
      resp   <- client.run(getChunkRequest)
    } yield resp

    result.use(resp => resp.body.through(text.utf8.decode).compile.toList)
  }

}
