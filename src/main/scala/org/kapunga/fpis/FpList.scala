package org.kapunga.fpis

sealed trait FpList[+A]
case object FpNil extends FpList[Nothing]
case class Cons[+A](head: A, tail: FpList[A]) extends FpList[A]

object FpList {
  // Exercise 3.2
  def tail[A](list: FpList[A]): FpList[A] = list match {
    case Cons(x, xs) => xs
    case FpNil       => FpNil
  }

  // Exercise 3.3
  def setHead[A](a: A, list: FpList[A]): FpList[A] = list match {
    case Cons(x, xs) => Cons(a, xs)
    case FpNil       => Cons(a, FpNil)
  }

  // Exercise 3.4
  def drop[A](l: FpList[A], n: Int): FpList[A] =
    if (n <= 0) l
    else {
      l match {
        case Cons(x, xs) => drop(xs, n - 1)
        case FpNil       => FpNil
      }
    }

  // Exercise 3.5
  def dropWhile[A](l: FpList[A], f: A => Boolean): FpList[A] =
    l match {
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
      case FpNil       => FpNil
    }

  // Exercise 3.6
  def init[A](l: FpList[A]): FpList[A] = l match {
    case Cons(x, FpNil) => FpNil
    case Cons(x, xs)    => Cons(x, init(xs))
    case FpNil          => FpNil
  }

  def sum(ints: FpList[Int]): Int = ints match {
    case FpNil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FpList[Double]): Double = ds match {
    case FpNil        => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // Exercise 3.10
  def foldLeft[A, B](as: FpList[A], z: B)(f: (B, A) => B): B =
    as match {
      case FpNil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f) 
    }

  def foldRight[A, B](as: FpList[A], z: B)(f: (A, B) => B): B =
    as match {
      case FpNil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: FpList[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: FpList[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: FpList[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  def sum3(ns: FpList[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: FpList[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: FpList[A]): Int = foldLeft(as, 0)((y, x) => 1 + y)

  // Exercise 3.12
  def reverse[A](as: FpList[A]): FpList[A] = foldLeft[A, FpList[A]](as, FpNil)((list, elem) => Cons(elem, list))

  // Exercise 3.13
  def foldRight2[A, B](as: FpList[A], z: B)(f: (A, B) => B): B = {
    val rl = foldLeft[A, FpList[A]](as, FpNil)((list, elem) => Cons(elem, list))
    foldLeft(rl, z)((a, b) => f(b, a))
  }

  // Exercise 3.14
  def append[A](a: FpList[A], b: FpList[A]): FpList[A] = foldRight2(a, b)((x, xs) => Cons(x, xs))

  // Exercise 3.15
  def flatten[A](as: FpList[FpList[A]]): FpList[A] = foldRight2(as, FpNil:FpList[A])(append(_, _)) 

  // Exercise 3.16
  def addOne(as: FpList[Int]): FpList[Int] = foldRight2(as, FpNil:FpList[Int])((x, xs) => Cons(x + 1, xs))

  // Exercise 3.17
  def doubleToString(as: FpList[Double]): FpList[String] = foldRight2(as, FpNil:FpList[String])((x, xs) => Cons(x.toString, xs))

  // Exercise 3.18
  def map[A, B](as: FpList[A])(f: A => B): FpList[B] = foldRight2(as, FpNil:FpList[B])((x, xs) => Cons(f(x), xs))

  // Exercise 3.19
  def filter[A](as: FpList[A])(f: A => Boolean): FpList[A] = foldRight2(as, FpNil:FpList[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  // Exercise 3.20
  def flatmap[A, B](as: FpList[A])(f: A => FpList[B]): FpList[B] = foldRight2(as, FpNil:FpList[B])((x, xs) => append(f(x), xs))

  // Exercise 3.21
  def filterViaFlatmap[A](as: FpList[A])(f: A => Boolean): FpList[A] = flatmap(as)(x => if (f(x)) FpList(x) else FpNil)

  // Exercise 3.22
  def mergeValues[Int](a: FpList[Int], b: FpList[Int]): FpList[Int] = ??? 
  
  // Exercise 3.23
  def zipWith[A](a: FpList[A], b: FpList[A])(f: (A, A) => A): FpList[A] = ???


  def apply[A](as: A*): FpList[A] =
    if (as.isEmpty) FpNil
    else Cons(as.head, apply(as.tail: _*))
}
