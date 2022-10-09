package module3.zio_homework

import zio.{ExitCode, URIO, ZEnv}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    SixthTask.runApp.exitCode
}
