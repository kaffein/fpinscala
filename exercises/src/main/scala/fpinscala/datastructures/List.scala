package fpinscala.datastructures

import scala.annotation.tailrec

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

  /*
   Same here, we use pattern matching to match and deconstruct the subexpressions from the original list. Since we
   would like to replace the header with a new value (i.e : we don't even need its original value to be used for processing the
    new header value), we do not need to match its old value, hence the use of '_' to catch it.
    On the other hand, the tail of the original list should remain the same, hence the use of a named variable (here t) to
    catch its value because it will be used on the right-hand side of => to produce the list new value with the new header.
    The only thing remaining is to return a new list via the Cons data constructor using the new header value and the retained
    original list tail.
   */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    // We use this case to handle the case where we have a nil list
    case Nil => sys.error("Can not add an element to a nil list")
  }

  /*
   Using the tail function defined earlier, we can generalize its use for recursively emulating the dropping of n elements
   from the head of a list with pattern matching.
   In fact, dropping an element from a list consists in pattern matching its different constituents based on the Cons data
   constructor we defined, which gives the head part and the tail part, and finally returning only the tail part (i.e : the original
   list minus the first element of the list)
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    // n negative or zero means no op
    if (n <= 0) l
    else {
      l match {
        case Cons(_, t) => drop(t, n - 1)
        // again, this is here to handle the corner case of a Nil list
        case Nil => Nil
      }
    }
  }

  /*
   We could have implemented drop using a more 'imperative' and lower-level approach via recursion by defining a function
   which contains the logic for looping recursively through the list.
   In Scala, it is possible to define functions inside functions. Intuitively, the idea is to define the scope of the
   inner function to be the outter function so that the inner function is only accessible within the context of the outter function
   and can only be used locally to that function.
   This is useful when there is a need to define a function as a small utility but whose usefulness or reusability outside the scope of
   the outter function is not apparent or obvious (yep otherwise, we could have put it at the outter level which would have made it
   accessible and usable by other functions). We use this inner function here to define the recursive loop logic.
   */
  def drop2[A](l: List[A], n: Int): List[A] = {

    /*
     When writing the recursive call, the Scala compiler was capable of inferring the fact that we were writing a function that is tail recursive.
     A tail recursive function is a function which has a call to itself as its last action. That is the case here with the 'else' branch.
     In that particular case, the compiler will optimize the generated bytecode to be the same as a classic while loop bytecode, hence providing
     the same kind of performance associated with classic loops and also preventing the consumption of stack frames normally associated with
     recursion calls. It is enforced by the @tailrec annotation here.

     On the other hand, if the last action have involved more than calling the function itself (e.g : loop(tail(list), counter - 1) * 2))) -- here
     with the * 2 for example --, then the call is not tail recursive and the compiler will not be able to optimize the bytecode which incidentally
     makes the function call consume stack frames like regular non tail-recursive function calls. As a consequence, a deep enough recursion, consuming
     a lot of stack frames would be able to blow up the whole stack with the infamous StackOverflow error if there is no more frame left.
     */
    @tailrec
    def loop(list: List[A], counter: Int): List[A] = {
      if (counter <= 0) list
      else loop(tail(list), counter - 1)
    }

    loop(l, n)
  }

  /*
    The idea of looping through a collection construct and have a predicate as a condition to continue(or not) looping through it naturally and intuitively
    lead (like in the previous example) to the combination of pattern-matching and recursion as means of achieving the result.
    Pattern-matching allows the deconstruction of the expression to be looped through, giving the different components of the expression and the predicate
    will be used in the case of a matched pattern to trigger the recursion (i.e loop continuation) and processing of the remaining/processed list or not,
    which in the latter case returns the last processed value of the list from the recursion pipeline.
  */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    // This function is a tail-recursive one since we only have dropWhile(t, f) as the last instruction in the first case, hence the use of the @tailrec
    // annotation
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    case Nil => Nil
  }

  /*
    We can also introduce a little implementation variation to the dropWhile function.
    In Scala, it is possible to pattern-match the scrutinee and use a 'guard' expression, a condition expression introduced by if, as an additional condition
    layer based on value, to the purely structural pattern matching introduced by 'case'.
    Therefore, it is possible to have pattern-matching based on :
     - structure-only : introduced by the 'case'-s
     - value : which is an ADDITION to the structural pattern matching, introduced by the 'guard' expression
  */
  @tailrec
  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile2(t, f) // This function is also tail-recursive, hence the use of @tailrec
    case _ => l
  }

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
