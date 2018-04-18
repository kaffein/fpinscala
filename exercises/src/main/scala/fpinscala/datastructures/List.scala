package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // The result will be 3 since the 'first' case that matches the scrutinee List(1,2,3,4,5)
  // is Cons(x, Cons(y, Cons(3, Cons(4, _))))
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    // Let's note however that the scrutinee could have been matched by the following 'case Cons(h, t)' as well (if the previous line were not there ^^)
    // which could have resulted in the value 15 being returned (it is an arithmetic series of progression 1 with 5 elements)
    // but since pattern matching stops at the first match, the previous case has been retained
    case Cons(h, t) => h + sum(t)
    // Same here, _ being a 'match all' expression, 'case _' can also be matched against the scrutinee List(1,2,3,4,5) but the way
    // the pattern matching mechanism works under the hood prevents it from going further than the 'case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y'
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
   The solution consists in pattern matching on the List scrutinee and returning the tail part
   of the Cons data constructor matched subexpressions.
   This operation is achieved in constant time since it only (always) involves removing the first element
   of the list.
  */
  def tail[A](l: List[A]): List[A] = l match {
    // Since we are only interested in having the tail part as a result (i.e : the head-matched part will not be
    // used on the right-hand side of =>), we match the head of the list with _, which is the match-all pattern
    // (as previously seen) to convey the idea that we are not (that) interested in using its value after
    // => even though theoretically, we could. _ is just a 'convention' for naming matched expressions that are not
    // essential for processing the value to be returned for the matched case.
    case Cons(_, t) => t
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
