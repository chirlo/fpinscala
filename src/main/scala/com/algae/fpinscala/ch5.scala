package com.algae.fpinscala

/**
 * @author iago
 */
object ch5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A]
    def take(i: Int): Stream[A]
    def takeWhile(p: A => Boolean): Stream[A]

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _          => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean

    def headOptionViaFold: Option[A]

    import Stream._
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

    def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, acc) => cons(a, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, acc) => f(a).append(acc))

    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }
    def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
      case _                        => None
    }
    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

    def zipWith[B](sb: Stream[B]): Stream[(A, B)] = unfold((this, sb)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
      case _                            => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))

      case (Cons(ha, ta), Empty)        => Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb))        => Some((None, Some(hb())), (Empty, tb()))
      case _                            => None
    }

    def startsWith[B](s: Stream[B]): Boolean =
      this.zipWith(s).forAll(x => x._1 == x._2)

    def tails: Stream[Stream[A]] = unfold(this) {
      case s @ Cons(ha, ta) => Some((s, ta()))
      case _                => None
    }
    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
      val ts = foldRight((z, cons(z, empty[B]))) { (a, sb) =>
        sb match {
          case (b, s) =>
            val x = f(a, b)
            (x, cons(x, s))
        }
      }
      ts._2

    }
  }
  case object Empty extends Stream[Nothing] {

    def toList: List[Nothing] = Nil

    def take(i: Int) = this

    def takeWhile(p: Nothing => Boolean) = this

    def forAll(p: Nothing => Boolean): Boolean = true

    def headOptionViaFold: Option[Nothing] = None
  }
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    def toList: List[A] = h() :: t().toList

    def take(i: Int) = if (i == 0) Empty
    else Cons(h, () => t().take(i - 1))

    def takeWhile(p: A => Boolean) = foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) Stream.cons(a, b)
      else Empty
    }

    def forAll(p: A => Boolean): Boolean = p(h()) && t().forAll(p)

    def headOptionViaFold: Option[A] = foldRight(None: Option[A]) { (a, _) => Some(a) }

  }
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs(x: Int, y: Int): Stream[Int] = cons(x, fibs(y, x + y))

    val fibo = fibs(0, 1)

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty[A]
    }

    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

    def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (x, y) => Some(x, (y, x + y)) }

    def ones = constantViaUnfold(1)

  }

}