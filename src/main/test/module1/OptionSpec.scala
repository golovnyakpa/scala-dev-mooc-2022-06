package module1

import org.scalatest.flatspec.AnyFlatSpec
import module1.OptionTask.Option

class OptionSpec extends AnyFlatSpec {

  val testOpt1: Option[Int]    = Option(1)
  val testOpt2: Option[String] = Option("qwe")
  val testOpt3: Option[String] = Option.None
  val testOpt4: Option[Int]    = Option(42)

  "Option" should "give API to zip Options" in {
    val zippedOpt1: Option[(Int, String)] = Option((1, "qwe"))
    assert(testOpt1.zip(testOpt2) == zippedOpt1)

    assert(testOpt3.zip(testOpt4) == Option.None)
  }

  "and Option" should "be filtered" in {
    assert(testOpt1.filter(_ == 42) == Option.None)
    assert(testOpt1.filter(_ == 1) == Option(1))
    assert(testOpt3.filter(_ == "qwer") == Option.None)
    assert(testOpt4.filter(_ == 42) == Option(42))
  }

  "and Option" should "give API for not fail fast zip" in {
    assert(testOpt1.notFailFastZip(testOpt2) == Option((1, "qwe")))
    assert(testOpt3.notFailFastZip(testOpt4) == Option((Option.None, 42)))
  }
}
