import scalaz._
import Scalaz._

object HelloMonoid extends App {

  case class Hoge(cost: Int, cv: Int)

  implicit object HogeMonoid extends Monoid[Hoge] {
    def zero: Hoge = Hoge(0, 0)
    def append(r1: Hoge, r2: => Hoge): Hoge = Hoge(r1.cost + r2.cost, r1.cv + r2.cv)
  }

  Console println "hello monoid"

  Console println Hoge(100, 2) |+| Hoge(50, 3)

  val hs = Hoge(100, 2) :: Hoge(200, 3) :: Nil

  Console println hs.reduce(_ |+| _)

  Console println hs.foldLeft(HogeMonoid.zero)(_ |+| _)



  case class Foo(s: String, xs: List[Int])

  implicit object FooMonoid extends Monoid[Foo] {
    def zero: Foo = Foo("", List.empty)
    def append(r1: Foo, r2: => Foo): Foo = Foo(s"${r1.s} and ${r2.s}", r1.xs ::: r2.xs)
  }

  val fs = Foo("apple", List(1, 2)) :: Foo("ornge", List(5, 9)) :: Foo("banana", List(0)) :: Nil

  Console println fs.reduce(_ |+| _)

}
