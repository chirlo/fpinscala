package com.algae.fpinscala

/**
 * @author iago
 */
object ch1 {

  def fib(n: Int): Int =
    if (n > 1) n + fib(n - 1)
    else n

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.zip(as.tail).map(Function.tupled(ordered)).exists(!_)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}