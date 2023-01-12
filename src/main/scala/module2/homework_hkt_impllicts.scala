package module2

object homework_hkt_impllicts{

  /**
   * Доработать сигнатуру tupleF и реализовать его По итогу должны быть возможны
   * подобные вызовы val r1 = println(tupleF(optA, optB)) val r2 =
   * println(tupleF(list1, list2))
   */
  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit a: F[A] => Bindable[F, A], b: F[B] => Bindable[F, B]): F[(A, B)] =
    fa.flatMap(x => fb.map((x, _)))

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  object Bindable {
    implicit def optToBindable[A](opt: Option[A]): Bindable[Option, A] =
      new Bindable[Option, A] {
        override def map[B](f: A => B): Option[B]             = opt.map(f)
        override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
      }

    implicit def listToBindable[A](opt: List[A]): Bindable[List, A] =
      new Bindable[List, A] {
        override def map[B](f: A => B): List[B] = opt.map(f)
        override def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)
      }
  }

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val tupleOpt  = tupleF(optA, optB)
  val tupleList = tupleF(list1, list2)

  println(tupleOpt)  // Some((1,2))
  println(tupleList) // List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))

}
