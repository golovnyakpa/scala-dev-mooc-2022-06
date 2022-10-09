package module3

import module3.zioConcurrency.{eff, printEffectRunningTime}
import module3.zioZManaged.config
import module3.zio_homework.FifthTask.RunningTimeService
import module3.zio_homework.config.{AppConfig, load}
import zio.console._
import zio.random.{Random, _}
import zio._
import zio.clock.Clock
import zio.duration.durationInt

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

    private val isIntInCorrectRange: (Int, Int, Int) => IO[String, Int] = (a: Int, b: Int, n: Int) =>
      ZIO.fromEither(
        if (n >= a && n <= b) Right(n) else Left("Incorrect range")
      )

    private val notifyErrorAndAskAgain: String => URIO[Console, Unit] =
      s => zio.console.putStrLn(s).zipRight(printQuestion)//"Failed to parse int")

    private val readLineUntilCorrectInt: (Int, Int) => ZIO[Console, IOException, Int] = (a: Int, b: Int) =>
      zio.console.getStrLn.flatMap { input =>
        ZIO
          .fromTry(Try(input.toInt))
          .orElse(
            notifyErrorAndAskAgain("Failed to parse int")
              .zipRight(readLineUntilCorrectInt(a, b))
          )
          .flatMap(x => isIntInCorrectRange(a, b, x))
          .orElse(
            notifyErrorAndAskAgain("Not correct range")
              .zipRight(readLineUntilCorrectInt(a, b))
          )
      }

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

  // не смог придумать, как оттестировать такую функцию, поэтому в сигнатуру добавил
  // ещё функцию преобразования Success Type эффекта
  object SecondTask {

    def doWhile[R, E, A](
      eff: ZIO[R, E, A],
      pred: A => Boolean,
      changeSuccess: A => A = identity[A](_)
    ): ZIO[R with Console, E, A] =
      eff.flatMap { x =>
        if (pred(x))
          ZIO.succeed(x)
        else {
          zio.console.putStrLn("Predicate is false").zipRight(doWhile(eff.map(changeSuccess), pred, changeSuccess))
        }
      }

    def doWhileWithPrintingFinalValue[R, E, A](
      eff: ZIO[R, E, A],
      pred: A => Boolean,
      changeSuccess: A => A = identity[A](_)
    ): ZIO[R with Console, E, Unit] =
      doWhile(eff, pred, changeSuccess).flatMap(x => zio.console.putStrLn(s"Finally done with ${x.toString}"))
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в
   * случае ошибки вернет дефолтный конфиг и выведет его в консоль Используйте
   * эффект "load" из пакета config
   */

  object ThirdTask {

    private val defaultConfig = AppConfig("127.0.0.1", "1337")

    def loadConfigOrDefault =
      load.orElse(ZIO.succeed(defaultConfig))
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ, обратите
   * внимание на сигнатуры эффектов, которые будут у вас получаться, на
   * изменение этих сигнатур
   */
  object FourthTask {

    /**
     * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное
     * число от 0 до 10 спустя 1 секунду Используйте сервис zio Random
     */
    private lazy val getRandomNumEffect: ZIO[Random with Clock, Nothing, Int] =
      ZIO.sleep(1 seconds).zipRight(zio.random.nextIntBetween(1, 11))

    /**
     * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
     */
    private lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] =
      List.fill(10)(getRandomNumEffect)

    /**
     * 4.3 Напишите программу которая вычислит сумму элементов коллекци
     * "effects", напечатает ее в консоль и вернет результат, а также залогирует
     * затраченное время на выполнение, можно использовать ф-цию
     * printEffectRunningTime, которую мы разработали на занятиях
     */

    private val accInitialValue: ZIO[Random with Clock, Nothing, Int] = ZIO.succeed(0)
    private val reducedList: ZIO[Random with Clock, Nothing, Int] = effects.foldLeft(accInitialValue) { (acc, curr) =>
      for {
        a <- acc
        c <- curr
      } yield a + c
    }

    lazy val app: ZIO[Console with Clock with Random, Nothing, Int] = for {
      sum <- printEffectRunningTime(reducedList)
      _   <- zio.console.putStrLn(sum.toString)
    } yield sum

    /**
     * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее
     * выполнения
     */

    private lazy val effectsTest: List[ZIO[Random with Clock, Nothing, Int]] =
      List.fill(10)(ZIO.succeed(2))
    private lazy val calculationsSpeedUp: ZIO[Random with Clock, Nothing, Int] =
      ZIO.reduceAllPar(accInitialValue, effects)(_ + _)

    lazy val appSpeedUp: ZIO[Console with Clock with Random, Nothing, Int] = for {
      fastSum <- printEffectRunningTime(calculationsSpeedUp)
      _       <- zio.console.putStrLn(fastSum.toString)
    } yield fastSum

  }

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в
   * отдельный сервис, так чтобы ее молжно было использовать аналогично
   * zio.console.putStrLn например
   */
  object FifthTask {

    type RunningTimeService = Has[RunningTimeService.Service]

    object RunningTimeService {
      trait Service {
        def logRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
      }

      class RunningTimeServiceImpl extends Service {
        override def logRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
          printEffectRunningTime(eff)
      }

      val live: ULayer[Has[Service]] = ZLayer.succeed(new RunningTimeServiceImpl())

    }

  }

  /**
   * 6. Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет
   * логировать время выполнения прогаммы из пункта 4.3
   */

  object SixthTask {

    lazy val appWithTimeLogg: ZIO[Console with Clock with Random with RunningTimeService, Nothing, Unit] = for {
//      _ <- ZIO.accessM[RunningTimeService with Console with Clock with Random](_.get.logRunningTime(FourthTask.app))
      runTimeService <- ZIO.environment[FifthTask.RunningTimeService].map(_.get)
      _              <- runTimeService.logRunningTime(FourthTask.app)
    } yield ()

    /**
     * Подготовьте его к запуску и затем запустите воспользовавшись
     * ZioHomeWorkApp
     */

    lazy val runApp: ZIO[Console with Clock with Random, Nothing, Unit] =
      appWithTimeLogg.provideSomeLayer[Console with Clock with Random](RunningTimeService.live)
  }

}
