package com.algae.fpinscala

/**
 * @author iago
 */
object ch4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(get))
    def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    def getOrElse[B >: A](default: => B): B = get
    def orElse[B >: A](ob: => Option[B]): Option[B] = this
    def filter(f: A => Boolean): Option[A] = if (f(get)) this else None

  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = None
    def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    def getOrElse[B >: Nothing](default: => B): B = default
    def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
    def filter(f: Nothing => Boolean): Option[Nothing] = None
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      val vari = xs.map(x => math.pow(x - m, 2))
      mean(vari)
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((x, o) => x match {
      case Some(l) => o.map(l :: _)
      case _       => None
    })

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((x, o) => f(x) match {
      case Some(l) => o.map(l :: _)
      case _       => None
    })

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((x, o) => map2(f(x), o)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }
  case class Left[+E](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B): Either[E, B] = this
    def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
    def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
  }
  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
    def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(bb => f(value, bb))
  }

  def traverseE[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, el) => f(a).map2(el)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverseE(es)(identity)
}