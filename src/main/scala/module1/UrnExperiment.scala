package module1

import scala.util.Random

case class UrnExperiment() {

  private val urn: List[Int] = List(1, 1, 1, 0, 0, 0)
  private val rnd: Random    = new scala.util.Random

  /**
   * Получаем шар и обновляем состояние урны (возвращаем новую урну без
   * выбранного шара)
   */
  private case class BallExtractionResult(newUrn: List[Int], ballColor: Int)

  private def getRandomBall(urn: List[Int]): BallExtractionResult = {
    val rndIdx: Int    = rnd.nextInt(urn.size)
    val ballColor: Int = urn(rndIdx)
    val newUrn: List[Int] = urn.zipWithIndex.filter { case (_, idx) =>
      idx != rndIdx
    }.map(_._1)
    BallExtractionResult(newUrn, ballColor)
  }

  def runExperiment: Boolean = {
    val fstExtractionResult: BallExtractionResult = getRandomBall(urn)
    val sndExtractionResult                       = getRandomBall(fstExtractionResult.newUrn)
    if (fstExtractionResult.ballColor == 0 && sndExtractionResult.ballColor == 1) true else false
  }

}
