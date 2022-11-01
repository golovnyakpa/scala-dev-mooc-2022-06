package module4.http4s_homework

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

object Models {

  case class Counter(counter: Long)
  implicit val encoderCounter: Encoder[Counter] =
    deriveEncoder[Counter]
  implicit val decoderCounter: Decoder[Counter] =
    deriveDecoder[Counter]

}
