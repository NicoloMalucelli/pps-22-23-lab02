package u02

import u02.Modules.Person

import scala.math.{Pi, abs}

object Task extends App{

  // Task 2a

  val positive1 = (x: Int) => x match
    case y if y >= 0 => "positive"
    case _ => "negative"

  def positive2(x: Int): String = x match
    case y if y >= 0 => "positive"
    case _ => "negative"

  println("____TEST positive 1____")
  println(positive1(-1)=="negative")
  println(positive1(1)=="positive")

  println("____TEST positive 2____")
  println(positive2(-1)=="negative")
  println(positive2(1)=="positive")

  val empty: String => Boolean = _ == ""

  val not1 = (p: String => Boolean) => (x: String) => x match
    case y if p(y) => false
    case _ => true

  println("____TEST not 1____")
  println(not1(empty)("")==false)
  println(not1(empty)("x")==true)

  def not2(p: String => Boolean): String => Boolean = (x: String) => x match
    case y if p(y) => false
    case _ => true

  println("____TEST not 2____")
  println(not2(empty)("")==false)
  println(not2(empty)("x")==true)

  def not3[T](p: T => Boolean): T => Boolean = (x:T) => x match
    case y if p(y) => false
    case _ => true

  println("____TEST not generics____")
  val pos = (x:Int) => x >= 0
  println(not3(pos)(-1)==true)
  println(not3(pos)(1)==false)

  // Task 2b

  val p1 : Int => Int => Int => Boolean =  x => y => z => x <= y && y == z

  println("____TEST p1____")
  println(p1(1)(2)(2)==true)
  println(p1(1)(2)(1)==false)

  val p2 = (x: Int, y: Int, z: Int) => x <= y && y == z

  println("____TEST p2____")
  println(p2(1, 2, 2)==true)
  println(p2(1, 2, 1)==false)

  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  println("____TEST p3____")
  println(p3(1)(2)(2)==true)
  println(p3(1)(2)(1)==false)

  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  println("____TEST p2____")
  println(p4(1, 2, 2)==true)
  println(p4(1, 2, 1)==false)

  def compose[T](f: T => T, g: T => T): T => T = (x: T) => f(g(x))

  println("____compose____")
  val square = (x: Int) => x*x
  println(compose(square, square)(2)==16)
  println(compose(square, square)(3)==81)

  // Task 3

  def GCD(x: Int, y: Int): Int = (x, y) match
    case (v1, v2) if v2 == 0 => v1
    case (v1, v2) if v1 > v2 => GCD(v2, v1%v2)
    case (v1, v2) => GCD(v1, v2%v1)

  println("____GCD____")
  println(GCD(12, 8)==4)
  println(GCD(7, 14)==7)

  // Task 4

  enum Shape:
    case Rectangle(b: Double, h: Double)
    case Square(l: Double)
    case Circle(r: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(b, h) => 2*b + 2*h
      case Square(l) => 4*l
      case Circle(r) => 2*r*Pi

    import scala.math._

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(b, h), (x, y)) => x >= -b/2 && x <= b/2 && y >= -h/2 && y <= h/2
      case (Square(l), (x, y)) => x >= -l/2 && x <= l/2 && y >= -l/2 && y <= l/2
      case (Circle(r), (x, y)) => x*x + y*y < r*r

  import Shape.*

  println("____Perimeter____")
  println(abs(perimeter(Rectangle(10, 3)) - 26) <= 0.0001 )
  println(abs(perimeter(Square(10)) - 40) <= 0.0001)
  println(abs(perimeter(Circle(5)) - 31.4159265359) <= 0.0001)

  println("____Contains____")
  println(contains(Rectangle(10, 3), (1, 1))==true)
  println(contains(Rectangle(10, 3), (-6, 0))==false)
  println(contains(Square(10), (1, 1))==true)
  println(contains(Square(10), (-6, 0))==false)
  println(contains(Circle(5), (1, 1))==true)
  println(contains(Circle(5), (-6, 0))==false)

  //Task 5

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:
    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(p: A => Boolean): Option[A] = opt match
      case Some(a) if p(a) => Some(a)
      case _ => None()

    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match
      case Some(a) => Some(f(a))
      case _ => None()

    def fold[A](opt: Option[A])(orElse: A)(f: A => A): A = opt match
      case Some(a) => f(a)
      case _ => orElse

  import Option.*

  println("____filter____")
  println(filter(Some(5))(_ > 2) == Some(5))
  println(filter(Some(5))(_ > 8) == None())
  println(filter(None[Int]())(_ > 2)  == None())

  println("____map____")
  println(map(Some(5))(_ > 2) == Some(true))
  println(map(Some(5))(_ > 8) == Some(false))
  println(map(None[Int]())(_ > 2) == None())

  println("____fold____")
  println(fold(Some(5))(1)(_ + 1) == 6)
  println(fold(None[Int]())(1)(_ + 1) == 1)

}