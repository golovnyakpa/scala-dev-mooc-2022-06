package module3

import zio.console._
import zio.random._
import zio._

import java.io.IOException
import scala.language.postfixOps
import scala.util.Try

package object zio_homework {

  /**
   *   1. Используя сервисы Random и Console, напишите консольную ZIO программу
   *      которая будет предлагать пользователю угадать число от 1 до 3 и
   *      печатать в консоль угадал или нет. Подумайте, на какие наиболее
   *      простые эффекты ее можно декомпозировать.
   */

  object FirstTask {

    private val getRandomNumber: (Int, Int) => URIO[Random, Int] =
      (a: Int, b: Int) => zio.random.nextIntBetween(a, b + 1) // 1. Сгенерировать число

    private val printQuestion: URIO[Console, Unit] = zio.console.putStrLn("Enter number from 1 to 3")

    private def compareNums(userNum: Int, hiddenNum: Int): Task[String] =
      ZIO.effect(
        if (userNum == hiddenNum) s"Congrats! $userNum is correct answer."
        else s"You lose. Hidden num was $hiddenNum, your num was $userNum."
      )

    val naiveGuessProgram: ZIO[Console with Random, Throwable, Unit] = for {
      rn            <- getRandomNumber(1, 3)
      _             <- printQuestion
      userNum       <- zio.console.getStrLn
      comparisonRes <- compareNums(userNum.toInt, rn)
      _             <- zio.console.putStrLn(comparisonRes)
    } yield ()

    /**
     * No errors handling here, it's a pity. Possible errors:
     * 1.User entered not number. 2. User entered number not from required
     * range. Handling for 1: print message that user entered not number and ask
     * him to enter it again Handling for 2: print message that user entered not
     * required number and ask him to enter it again
     */

    private val notifyIntParsingError: URIO[Console, Unit] = zio.console.putStrLn("Failed to parse int")

    private val readLineUntilCorrectInt: (Int, Int) => ZIO[Console, IOException, Int] = (a: Int, b: Int) =>
      zio.console.getStrLn.flatMap { input =>
        ZIO
          .fromTry(Try(input.toInt))
          .orElse(
            notifyIntParsingError
              .zipRight(printQuestion)
              .zipRight(readLineUntilCorrectInt(a, b))
          )
          .flatMap(x => isIntInCorrectRange(a, b, x))
          .orElse(
            zio.console
              .putStrLn("Not correct range")
              .zipRight(printQuestion)
              .zipRight(readLineUntilCorrectInt(a, b))
          )
      }

    private val isIntInCorrectRange: (Int, Int, Int) => IO[String, Int] = (a: Int, b: Int, n: Int) =>
      ZIO.fromEither(
        if (n >= a && n <= b)(Right(n)) else Left("Incorrect range")
      )

    private val allowedRange: (Int, Int) = (1, 3)

    val errorHandlingGuessProgram: ZIO[Console with Random, Throwable, Unit] = for {
      rn            <- getRandomNumber(allowedRange._1, allowedRange._2)
      _             <- printQuestion
      userNum       <- readLineUntilCorrectInt(allowedRange._1, allowedRange._2)
      comparisonRes <- compareNums(userNum, rn)
      _             <- zio.console.putStrLn(comparisonRes)
    } yield ()

  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять
   * эффект до тех пор, пока его значение в условии не даст true
   */

  object SecondTask {
    def doWhile[R, E, A](eff: ZIO[R, E, A], pred: A => Boolean): ZIO[R, E, A] =
      eff.flatMap(x => if (pred(x)) ZIO.succeed(x) else doWhile(eff, pred))
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в
   * случае ошибки вернет дефолтный конфиг и выведет его в консоль Используйте
   * эффект "load" из пакета config
   */

  object ThirdTask {

    case class Config(config: String)

    def loadConfigOrDefault(configFile: String, defaultConfig: String): Config = {

    }
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ, обратите
   * внимание на сигнатуры эффектов, которые будут у вас получаться, на
   * изменение этих сигнатур
   */

  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное
   * число от 0 до 10 спустя 1 секунду Используйте сервис zio Random
   */
  lazy val eff = ???

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = ???

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное
   * время на выполнение, можно использовать ф-цию printEffectRunningTime,
   * которую мы разработали на занятиях
   */

  lazy val app = ???

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее
   * выполнения
   */

  lazy val appSpeedUp = ???

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в
   * отдельный сервис, так чтобы ее молжно было использовать аналогично
   * zio.console.putStrLn например
   */

  /**
   * 6. Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет
   * логировать время выполнения прогаммы из пункта 4.3
   */

  lazy val appWithTimeLogg = ???

  /**
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp = ???

}
