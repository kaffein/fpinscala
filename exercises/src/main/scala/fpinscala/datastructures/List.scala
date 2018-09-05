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
  /*
   EXERCISE 3.10
   `foldRight` is not `stack-safe` here. In fact, its last action `is not` a call to itself but instead consists in a call to f with `x` as
   the first argument and `foldRight` as the second argument as shown in the `Cons matched`-case returned value : `f(x, foldRight(xs, z)(f))`
   */

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  /*
   EXERCISE 3.7
  `product` using foldRight, like `sum`, does not halt the recursion and return early if it encounters a 0.0 because the way `foldRight` is implemented, the recursion
   has to be carried out to the end of the list before it begins collapsing it.
  */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
   EXERCISE 3.8
   Passing Nil and `Cons` to `foldRight` like this :
   foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) would give the following result after the substitution steps :

   Cons(1, foldRight(List(2, 3), Nil)(Cons(_, _)) << but since List(2, 3) = Cons(2, Cons(3, Nil)), proceeding with the next substitution step gives
   Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil)(Cons(_,_))) << Forgot the type hint for foldRight `z` param at first though so the compiler complained because
                                                             << it could not infer the type. It would have been okay if we had the parameters in two separate groups
   Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_,_))) << Added the type hint `List[Int]` to help the compiler with type inference


   If we proceed with the substitution steps as we go through the recursion :
   foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

   Cons(1, foldRight(List(2, 3, Nil)(Cons(_, _)))
   Cons(1, Cons(2, foldRight(List(3), Nil)(Cons(_, _))))
   Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)(Cons(_, _))))) << but since `foldRight` parameter `as` (scrutinee) matches Nil in foldRight(Nil, Nil)(Cons(_, _)), foldRight returns Nil
   Cons(1, Cons(2, Cons(3, Nil))) >> we then get back the original List and the whole process seemed like an `identity` function
  */

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

  /*
   Given the implementation of the List data structure (Singly-linked list), retrieving a list consisting of all but the last element from a List is not possible
   in `constant time`. In fact, when we want to replace the tail of a singly-linked list, we deconstruct the list allowing direct access to its `head` and
   `tail` elements.
   But since the `tail` element is still a nested data structure (nested `Cons`) consisting in the remaining of the list, we have to `recursively` deconstruct
   the `tail` element into a successive stack of `Cons` elements and retain each `Cons` element as we go through, until reaching the one whose `tail` is
   pointing to `Nil` i.e the last `Cons` element not to be taken into account when computing `init`. We then return all the `Cons` elements that we have
   accumulated so far into a List as the result of `init`.

   Hence, the time it takes to compute `init` is then `proportional` to the size of the List.
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can not compute init of an empty list")
    case Cons(_, Nil) => l
    case Cons(h, t) => Cons(h, init(t))
  }

  /*
   This function can not be implemented in constant time because it involves traversing the entire list from head to the last element in order to build the
   new list. Furthermore, the function is not tail-recursive since it involves doing a Cons operation in addition to the call to init on the remaining of the
   list, at each step. It then consumes a stack frame for each step of the recursion and may lead to the infamous StackOverflow error to be thrown if the list is large
   enough.

   Writing purely functional data structures supporting different operations efficiently is about finding clever ways to exploit data sharing. Sometimes,
   trade-offs have to be made.
   */

  /*
   First, a little reminder of how we have implemented `foldRight` so far :

   def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

   Using `foldRight` to process the length of a list would then consists in :
     - providing the element to be returned when the list matches Nil
     - providing a function, taking two arguments, to be called to add an element to the result when the list matches Cons

   Since our intent is to process the length of a list, we will go through the recursion and at each step, we will add one (1) until we reach the end of the list
   which happens when the scrutinee matches Nil. In which case, we return 0 instead of adding 1 since we have exhausted the list. From there, foldRight will
   have all its parameters resolved and it will begin to collapse the nested expressions by applying the function f.
     - the element to be returned on Nil match is then : 0 (initial value of our counter)
     - the function to be used has to `increment` and `accumulate` the value as we go through the recursion giving us the function (a, b) => (1 + b)
   */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => 1 + b)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  /*
   When implementing `sum`and `product` on `foldLeft`, the `z` parameter will act as an `accumulator` and will be provided with an initial value (also called a `seed`)
   according to the operation to be implemented : 0 for `sum` and `1.0` for `product`.
   These values corresponds respectively to the `neutral element` or `identity element` of `sum` and `product`.
   */
  def sum3[A](as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product3[A](as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  /*
   Same thing for `length`, the `accumulation` function consists in adding 1 to the current value of the accumulator (`b` parameter in (b, a) => b + 1) each time we
   match a `Cons` (i.e the list is not exhausted yet). The `seed` value of the parameter will be 0 since this is the initial length of an empty List and we keep adding
   1 to this value until we reach the end of the list, which corresponds to the `Nil`element.
   */
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((b, a) => b + 1)

  /*
   scala> List.product3(List(1, 2, 3, 4))
   res4: Double = 24.0

   scala> List.product3(List(1, 2, 3, 4, 0))
   res5: Double = 0.0

   scala> List.sum3(List(1, 2, 3, 4, 0))
   res6: Int = 10

   scala> List.length3(List(1, 2, 3, 4, 0))
   res21: Int = 5
   */

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
