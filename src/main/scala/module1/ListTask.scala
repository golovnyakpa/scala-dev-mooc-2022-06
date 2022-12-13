package module1

import scala.annotation.tailrec

object ListTask extends App {
  object list {

    /**
     * Реализовать односвязанный иммутабельный список List Список имеет два
     * случая: Nil - пустой список Cons - непустой, содердит первый элемент
     * (голову) и хвост (оставшийся список)
     */

    trait List[+T] {

      /**
       * Метод cons, добавляет элемент в голову списка, для этого метода можно
       * воспользоваться названием `::`
       */
      def ::[TT >: T](elem: TT): List[TT] =
        List.::(elem, this)

      /**
       * Метод mkString возвращает строковое представление списка, с учетом
       * переданного разделителя
       */
      def mkString(sep: String): String = {
        @tailrec
        def loop(lst: List[T], acc: String): String =
          lst match {
            case List.::(head, tail) => loop(tail, acc + head + sep)
            case List.Nil            => acc.dropRight(1)
          }
        loop(this, "")
      }

      /**
       * Реализовать метод reverse который позволит заменить порядок элементов в
       * списке на противоположный
       */
      def reverse: List[T] = {
        @tailrec
        def loop(lst: List[T], acc: List[T]): List[T] =
          lst match {
            case List.::(head, tail) => loop(tail, head :: acc)
            case List.Nil            => acc
          }
        loop(this, List[T]())
      }

      /**
       * Реализовать метод map для списка который будет применять некую ф-цию к
       * элементам данного списка
       */
      def map[B](f: T => B): List[B] = {
        @tailrec
        def loop(lst: List[T], acc: List[B]): List[B] =
          lst match {
            case List.::(head, tail) => loop(tail, f(head) :: acc)
            case List.Nil            => acc.reverse
          }
        loop(this, List[B]())
      }

      /**
       * Полезно для flatMap
       */
      def concat[TT >: T](anotherList: List[TT]): List[TT] = {
        @tailrec
        def loop(lst: List[TT], acc: List[TT]): List[TT] =
          lst match {
            case List.::(head, tail) => loop(tail, head :: acc)
            case List.Nil            => acc.reverse
          }
        loop(anotherList, this.reverse)
      }

      def flatMap[B](f: T => List[B]): List[B] = {
        @tailrec
        def loop(lst: List[T], acc: List[B]): List[B] =
          lst match {
            case List.::(head, tail) => loop(tail, f(head).concat(acc))
            case List.Nil            => acc.reverse
          }
        loop(this, List())
      }

      /**
       * Реализовать метод filter для списка который будет фильтровать список по
       * некому условию
       */
      def filter(p: T => Boolean): List[T] = {
        @tailrec
        def loop(lst: List[T], acc: List[T]): List[T] =
          lst match {
            case List.::(head, tail) => if (p(head)) loop(tail, head :: acc) else loop(tail, acc)
            case List.Nil            => acc.reverse
          }
        loop(this, List())
      }

    }

    object List {
      case class ::[A](head: A, tail: List[A]) extends List[A]

      case object Nil extends List[Nothing]

      /**
       * Конструктор, позволяющий создать список из N - го числа аргументов Для
       * этого можно воспользоваться *
       *
       * Например вот этот метод принимает некую последовательность аргументов с
       * типом Int и выводит их на печать def printArgs(args: Int*) =
       * args.foreach(println(_))
       */
      def apply[A](v: A*): List[A] =
        if (v.isEmpty) List.Nil else ::(v.head, apply(v.tail: _*))

      /**
       * Написать функцию incList котрая будет принимать список Int и возвращать
       * список, где каждый элемент будет увеличен на 1
       */
      def incList(lst: List[Int]): List[Int] = lst.map(_ + 1)

      /**
       * Написать функцию shoutString котрая будет принимать список String и
       * возвращать список, где к каждому элементу будет добавлен префикс в виде
       * '!'
       */
      def shoutString(lst: List[String]): List[String] = lst.map(x => s"!$x")

    }
  }
}
