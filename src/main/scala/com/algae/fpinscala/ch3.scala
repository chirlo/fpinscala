package com.algae.fpinscala

/**
 * @author iago
 */
object ch3 {

  sealed trait List[+A] {
    def tail: List[A]
  }

  case object Nil extends List[Nothing] {
    override def tail = ???
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def setHead[A](a: A, l: List[A]): List[A] = l match {
      case Cons(_, t) => Cons(a, t)
      case _          => l
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _                   => l
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil        => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(a, Nil) => Nil
      case Cons(a, h)   => Cons(a, init(h))
      case Nil          => Nil
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil         => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    //3.11
    def sumL(its: List[Int]): Int = foldLeft(its, 0)(_ + _)
    def productL(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
    def lengthL[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

    //3.12
    def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

    def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse(as), z)((a, b) => f(b, a))

    def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(as, identity: B => B)((g, a) => b => g(f(a, b)))(z)

    def appendFold[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons(_, _))

    def flatten[A](ll: List[List[A]]): List[A] =
      foldRight(ll, Nil: List[A])(appendFold)

    //3.16
    def plus1(l: List[Int]) = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

    def toString(l: List[Double]) = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      flatten(map(as)(f))

    def filterF[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] = (la, lb) match {
      case (Nil, _) | (_, Nil)        => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil)                             => true
      case (Nil, _)                             => false
      case (Cons(x, xs), Cons(y, ys)) if x == y => hasSubsequence(xs, ys)
      case _                                    => hasSubsequence(sup.tail, sub)
    }

  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size(t: Tree[_]): Int = t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case _            => 1
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(i)      => i
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth(t: Tree[_]): Int = t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

    def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }

    def fold[A, B](t: Tree[A])(z: B)(f: (A, B) => B): B = t match {
      case Leaf(a)      => f(a, z)
      case Branch(l, r) => fold(r)(fold(l)(z)(f))(f)
    }

    def sizeViaFold(t: Tree[_]): Int =
      fold(t)(0)((_, i) => i + 1)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(Int.MinValue)(_ max _)
      

  }

}