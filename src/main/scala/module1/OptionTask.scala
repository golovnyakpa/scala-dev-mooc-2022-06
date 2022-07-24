package module1

object OptionTask extends App {

  trait Option[+T] {

    def isEmpty: Boolean = this match {
      case Option.None    => true
      case Option.Some(v) => false
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None    => throw new Exception("get on empty Option")
    }

    def map[B](f: T => B): Option[B] =
      flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.None    => Option.None
      case Option.Some(v) => f(v)
    }

    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно
     * есть
     */
    def printIfAny: Unit =
      this match {
        case Option.Some(v) => println(v)
        case _              =>
      }

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option в
     * случае если исходный не пуст и предикат от значения = true
     */
    def filter(p: T => Boolean): Option[T] =
      this.flatMap(x => if (p(x)) Option(x) else Option.None)

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из
     * 2-х Option
     */
    def zip[B](anotherOpt: Option[B]): Option[(T, B)] =
      for {
        o1 <- this
        o2 <- anotherOpt
      } yield (o1, o2)

    def notFailFastZip[B](anotherOpt: Option[B]): Option[(T, B)] =
      if (this.isEmpty && anotherOpt.isEmpty)
        Option((Option.None.asInstanceOf[T], Option.None.asInstanceOf[B]))
      else if (!this.isEmpty && anotherOpt.isEmpty)
        Option(this.get, Option.None.asInstanceOf[B])
      else if (this.isEmpty && !anotherOpt.isEmpty)
        Option(Option.None.asInstanceOf[T], anotherOpt.get)
      else
        this.zip(anotherOpt)

  }

  object Option {

    final case class Some[T](v: T) extends Option[T]
    final case object None         extends Option[Nothing]

    def apply[T](v: T): Option[T] = Some(v)

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из
     * 2-х Option
     */
    // Не до конца понял, поэтому сделал zip ещё в таком варианте, хотя не кажется, что такое
    // может кому-то понадобится, т к проще сразу в конструктор tuple передать
    def zip[A, B](opt1: Option[A], opt2: Option[B]): Option[(A, B)] =
      for {
        o1 <- opt1
        o2 <- opt2
      } yield (o1, o2)
  }

  val myOpt1 = Option(2)
  myOpt1.printIfAny
  val myOpt2                                     = Option("3")
  val res: Option[(Option[Int], Option[String])] = Option(myOpt1, myOpt2)
  val myRes: Option[(Int, String)]               = Option.zip(myOpt1, myOpt2)
  println(Option(myOpt1, myOpt2))

  val r: Option[(Int, String)] = myOpt1.zip(myOpt2)
  println(r)

  val none1: Option[String] = Option.None
  val none2: Option[Int]    = Option.None

  val result: Option[(String, Int)] = none1.notFailFastZip(none2)
  println(result)

}
