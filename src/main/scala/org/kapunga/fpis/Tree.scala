package org.kapunga.fpis

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(i) => i
      case Branch(l, r) => maximum(l) max maximum(r) 
    }
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
    }
  }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(fl: A => B)(fb: (B, B) => B): B = {
    tree match {
      case Leaf(a) => fl(a)
      case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
    }
  }

  def sizeFold[A](tree: Tree[A]): Int = fold[A, Int](tree)((a) => 1)((a, b) => 1 + a + b)

  def maxFold(tree: Tree[Int]): Int = fold[Int, Int](tree)(a => a)((a, b) => a max b)

  def depthFold[A](tree: Tree[A]): Int = fold[A, Int](tree)(a => 1)((a, b) => (a + 1) max (b + 1))

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))((a, b) => Branch(a, b))
}
