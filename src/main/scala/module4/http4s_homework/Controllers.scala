package module4.http4s_homework

import cats.data.{Validated, ValidatedNec}
import cats.effect.{IO, Ref}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.text
import module4.http4s_homework.Controllers.Validator.checkInputV
import module4.http4s_homework.Models.Counter
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.io._

import scala.concurrent.duration.DurationInt

object Controllers {

  def counterService(counter: Ref[IO, Long]): HttpRoutes[IO] =
    HttpRoutes.of { case GET -> Root / "counter" =>
      counter.getAndUpdate(_ + 1).flatMap(x => Ok(Counter(x)))
    }

  object Validator {

    private def isInt(s: String): Either[String, Int] =
      Either.cond(s.toIntOption.nonEmpty, s.toInt, "Not int")

    private def isPositiveInt(i: Int): Either[String, Int] =
      Either.cond(i >= 0, i, "Negative int")

    def checkInputV(s: String): Validated[String, Int] = {
      val res: Either[String, Int] = for {
        int        <- isInt(s)
        isPositive <- isPositiveInt(int)
      } yield isPositive
      Validated.fromEither(res)
    }

  }

  case class InputParams(chunk: String, total: String, time: String)
  private case class ParsedInputParams(chunk: Int, total: Int, time: Int)

  private def validateInputParams(params: InputParams): ValidatedNec[String, ParsedInputParams] =
    (
      checkInputV(params.chunk).leftMap(s => s"Chunk error: $s").toValidatedNec,
      checkInputV(params.total).leftMap(s => s"Total error: $s").toValidatedNec,
      checkInputV(params.time).leftMap(s => s"Time error: $s").toValidatedNec
    ).mapN { (chunk, total, time) =>
      ParsedInputParams(chunk, total, time)
    }

  val slowChunkService: HttpRoutes[IO] =
    HttpRoutes.of { case GET -> Root / "slow" / chunk / total / time =>
      val validationRes: ValidatedNec[String, ParsedInputParams] = validateInputParams(InputParams(chunk, total, time))
      validationRes match {
        case Validated.Invalid(nec) => BadRequest(nec.mkString_(" ").show)
        case Validated.Valid(parsedInput) =>
          val stream =
            Files[IO]
              .readRange(Path(PAYLOAD_FILE), parsedInput.chunk, 0, parsedInput.total)
              .through(text.utf8.decode)
              .evalMapChunk(chunk => IO.sleep(parsedInput.time.seconds).productR(IO.pure(chunk)))
          Ok(stream)
      }
    }

}
