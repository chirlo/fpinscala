package com.algae.fpinscala

import java.util.concurrent.TimeUnit
import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable
import java.util.concurrent.Future

object ch7_par {

  /*trait Future[A] {

    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }
*/
  //not a container, but a description of a computation
  type Par[A] = ExecutorService => Future[A]

  object Par {

    case class UnitFuture[A](g: A) extends Future[A] {
      def get = g
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    //will take care of starting execution, 'interprets' the computation
    //This is where purity ends!
    def run[A](s: ExecutorService)(a: Par[A]): A = a(s).get

    //7.1
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
      println(s"$pa <> $pb")

      /*val af = pa(es)
        println(Thread.currentThread.getId() + " af : " + af + " -" + af.get)
        val bf = pb(es)
        println(Thread.currentThread().getId() + " bf : " + bf + " -" + bf.get)

        //7.3 respect time_outs
        new UnitFuture[C](f(af.get, bf.get))
*/
      Map2(pa, pb, f)(es)
    }

    case class Map2[A, B, C](pa: Par[A], pb: Par[B], f: (A, B) => C) extends Par[C] {


      override def apply(es: ExecutorService): Future[C] = lift(f)(pa, pb)(es)
      override def toString = s"Map2($pa,$pb)"

    }

    def lift[A, B, C](f: (A, B) => C): (Par[A], Par[B]) => Par[C] = (pa, pb) => es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    //demands that computation be forked off the main Thread 
    //could be like async, hmmm...
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {

        def call = {
          val fa = a
          //println(Thread.currentThread().getId() + " forced a")

          fa(es).get

        }
      })

    //7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List[A]()))(map2(_, _)((x, y) => { println(s"Th-${Thread.currentThread().getId()} - join($x,$y)"); x :: y }))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      //this fork makes it return immediately ( the fold could take a long time for a big list)
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val ll = parMap(as)(a => if (f(a)) List(a) else List())
      map(ll)(_.flatten)
    }

    def parFold[A](as: IndexedSeq[A])(zero: A)(op: (A, A) => A): Par[A] = fork {
      if (as.size <= 1)
        unit(as.headOption.getOrElse(zero))
      else {

        val (l, r) = as.splitAt(as.length / 2)
        val sumL: Par[A] = parFold(l)(zero)(op)
        val sumR: Par[A] = parFold(r)(zero)(op)
        map2(sumL, sumR)(op)
      }
    }

    //7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      val ind = run(es)(n) // Full source files
      choices(ind)(es)
    }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    //7.12, 7.13
    def choice[A, B](n: Par[A])(f: A => Par[B]): Par[B] = es => {
      val ind = run(es)(n) // Full source files
      f(ind)(es)
    }

    //7.14
    //def join[A](ppa: Par[Par[A]]): Par[A] = es =>
    //ppa(es).get(es)

  }
}