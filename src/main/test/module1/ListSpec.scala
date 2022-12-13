package module1

import org.scalatest.flatspec.AnyFlatSpec
import module1.ListTask.list._

class ListSpec extends AnyFlatSpec {

  val lst1: List[Int]    = List(1, 2, 3, 4, 5, 6)
  val lst2: List[Char]   = List()
  val lst3: List[String] = List("q", "w", "e")

  "list" should "give ability to add at the first position" in {
    val newLst: List[Int] = lst1.::(0)
    assert(newLst == List(0, 1, 2, 3, 4, 5, 6))

    assert(42 :: lst2 == List(42))

    assert("r" :: "t" :: lst3 == List("r", "t", "q", "w", "e"))
  }

  "list" should "mkString correct" in {
    println(lst1.mkString("|"))
    assert(lst1.mkString("|") == "1|2|3|4|5|6")
  }

  "list" should "be reversable" in {
    assert(lst1.reverse == List(6, 5, 4, 3, 2, 1))
    assert(lst2.reverse == List())
    assert(lst3.reverse == List("e", "w", "q"))
  }

  "list" should "support map" in {
    assert(lst1.map(x => (x * 2).toString) == List("2", "4", "6", "8", "10", "12"))
    assert(lst2.map(_.isDigit) == List())
    assert(lst3.map(x => f"_${x}_") == List("_q_", "_w_", "_e_"))
  }

  "list" should "support filter" in {
    assert(lst1.filter(_ > 3) == List(4, 5, 6))
    assert(lst2.filter(_.isDigit) == List())
    assert(lst3.filter(_ == "w") == List("w"))
  }

  "list" should "support static incList" in {
    assert(List.incList(lst1) == List(2, 3, 4, 5, 6, 7))
    assert(List.incList(List(-1, -2, -3)) == List(0, -1, -2))
    assert(List.incList(List()) == List())
  }

  "list" should "support static shoutString" in {
    assert(List.shoutString(lst3) == List("!q", "!w", "!e"))
    assert(List.shoutString(List()) == List())
    assert(List.shoutString(List("silence")) == List("!silence"))
  }

  "list" should "support concat" in {
    assert(lst1.concat(List(7, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(lst2.concat(List('c')) == List('c'))
    assert(lst3.concat(List("rty", "ui", "op")) == List("q", "w", "e", "rty", "ui", "op"))
  }

  "list" should "support flatMap" in {
    assert(lst1.flatMap(x => List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
    assert(lst2.flatMap(_ => List('c')) == List())
    assert(lst3.flatMap(_ => List(true)) == List(true, true, true))
  }

}
