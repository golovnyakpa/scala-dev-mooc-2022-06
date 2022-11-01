package module3.cats_effect_homework

import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._
import module3.cats_effect_homework.Wallet.WalletId

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени.
// Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну
// секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и
// завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков,
// 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def addAmountPeriodically(
    wallet: Wallet[IO],
    amount: BigDecimal,
    period: FiniteDuration
  ): IO[Unit] =
    wallet
      .topup(amount)
      .flatMap(_ => IO.sleep(period))
      .flatMap(_ => addAmountPeriodically(wallet, amount, period))

  // не уверен, что такой способ печати предполагался
  def printWalletsAmount(wallets: List[Wallet[IO]]): IO[Unit] = {
    val res: IO[WalletId] = wallets.zipWithIndex.foldLeft(IO.pure("")) { case (acc, curr) =>
      for {
        a <- acc
        b <- curr._1.balance
      } yield a ++ s"Wallet ${curr._2} balance is: $b\n"
    }
    res.flatMap(IO.println).flatMap(_ => IO.sleep(1.seconds)).flatMap(_ => printWalletsAmount(wallets))
  }

  def run: IO[Unit] =
    for {
      _       <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      _ <- Spawn[IO].start(addAmountPeriodically(wallet1, BigDecimal(100), 200.millisecond))
      _ <- Spawn[IO].start(addAmountPeriodically(wallet2, BigDecimal(100), 500.millisecond))
      _ <- Spawn[IO].start(addAmountPeriodically(wallet3, BigDecimal(100), 2000.millisecond))
      _ <- Spawn[IO].start(printWalletsAmount(List(wallet1, wallet2, wallet3)))
      _ <- IO.readLine
    } yield ()

  // в целом работает, но иногда валится с IllegalNumberFormatException. Полагаю,
  // это из-за конкурентного доступа к файлам

}
