package it.unibo.pps.ex1

import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h) // pattern for scala.Option
    case _ => None // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldLeft(Nil())((list, value) => list.append(f(value)))

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = this match
    case h :: t => (h, value) :: t.zipWithValue(value)
    case _ => Nil()

  def zipWithValue2[B](value: B): List[(A, B)] = map(_ -> value)

  def length(): Int = this match
    case h :: t => 1 + t.length()
    case _ => 0

  def length2(): Int = foldLeft(0)((length, _) => length + 1)

  def indices(): List[Int] =
    def _indices(count: Int)(list: List[A]): List[Int] = list match
      case h :: t => count :: _indices(count + 1)(t)
      case _ => Nil()

    _indices(0)(this)

  def indices2(): List[Int] =
    val (_, result) = foldLeft[(Int, List[Int])]((length() - 1, Nil())) {
      case ((counter, list), _) => (counter - 1, counter :: list)
    }
    result

  def indices3(): List[Int] =
    val (_, result) = foldRight[(Int, List[Int])]((length() - 1, Nil())):
      case (_, (counter, list)) => (counter - 1, counter :: list)
    result

  def zipWithIndex: List[(A, Int)] =
    val (_, result) = foldRight[(Int, List[(A, Int)])]((length() - 1, Nil())):
      case (elem, (counter, list)) => (counter - 1, (elem, counter) :: list)
    result

  def reverse: List[A] =
    @tailrec
    def _reverse(l: List[A], acc: List[A]): List[A] = l match
      case h :: t => _reverse(t, h :: acc)
      case Nil() => acc

    _reverse(this, Nil())

  def partition(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _partition(remaining: List[A])(satisfied: List[A], notSatisfied: List[A]): (List[A], List[A]) = remaining match
      case h :: t if predicate(h) => _partition(t)(h :: satisfied, notSatisfied)
      case h :: t => _partition(t)(satisfied, h :: notSatisfied)
      case _ => (satisfied, notSatisfied)

    val (satisfied, notSatisfied) = _partition(this)(Nil(), Nil())
    (satisfied.reverse, notSatisfied.reverse)

  def partition2(predicate: A => Boolean): (List[A], List[A]) = foldRight[(List[A], List[A])]((Nil(), Nil())):
    case (elem, (satisfied, notSatisfied)) if predicate(elem) => (elem :: satisfied, notSatisfied)
    case (elem, (satisfied, notSatisfied)) => (satisfied, elem :: notSatisfied)

  def span(predicate: A => Boolean): (List[A], List[A]) = ???

  def takeRight(n: Int): List[A] = ???

  def collect(predicate: PartialFunction[A, A]): List[A] = ???

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main
def main(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithValue2(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.length()) // 4
  println(reference.length2()) // 4
  println(reference.indices()) // List(0, 1, 2, 3)
  println(reference.indices2()) // List(0, 1, 2, 3)
  println(reference.indices3()) // List(0, 1, 2, 3)
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.partition2(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)