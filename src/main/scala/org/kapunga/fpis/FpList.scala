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

  def apply[A](as: A*): FpList[A] =
    if (as.isEmpty) FpNil
    else Cons(as.head, apply(as.tail: _*))
}
