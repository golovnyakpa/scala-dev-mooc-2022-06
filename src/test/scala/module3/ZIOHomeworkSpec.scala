package module3

import module3.zio_homework.{FirstTask, SecondTask}
import zio.ZIO
import zio.test.{DefaultRunnableSpec, ZSpec}
import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestConsole

object ZIOHomeworkSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("First task")(
    testM("test console")(
      for {
        _ <- TestConsole.feedLines("4")
        _ <- FirstTask.errorHandlingGuessProgram
        value <- TestConsole.output
      } yield {
        assert(value)(equalTo(Vector("Not correct range", "Enter number from 1 to 3")))
      }
    ),
    suite("second task")(
      testM("doWhile predicate")(
        ???
//        assert(SecondTask.doWhile[Any, Nothing, Int](ZIO.succeed(2), _ > 2))(equalTo())
      )
    )
  )


}
